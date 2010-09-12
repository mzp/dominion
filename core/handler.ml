open Base
open ThreadUtils
open ListUtil
open Ccell

type 'a t = ('a * Protocol.response Event.channel * Protocol.game_request) Event.channel

let send ch e =
  Event.sync @@ Event.send ch e

let broadcast t e =
  List.map snd t#clients
  +> List.map (fun ch -> Event.send ch e)
  +> List.iter (ignore $ Event.poll)

let game t =
  let players =
    ListLabels.rev_map t#clients ~f:begin fun (name,_)->
      let (hands, decks) =
	Game.initial_hands
	+> ListUtil.shuffle
	+> HList.splitAt 5 in
	Game.make_player name ~hands ~decks
    end in
  let cards =
    List.concat [ Game.first_game;
		  Game.treasures;
		  Game.victories @@ List.length players]
  in
    Game.make players cards

let ready t ch pid id =
  if lookup pid t#names <> None then
    if List.mem pid t#ready then begin
      send ch @@ `Error (id, "already ready");
      t
    end else
      let t =
	t#ready <- pid :: t#ready in
	if List.length t#ready = List.length t#names then begin
	  let t =
	    t#game <- game t in
	  let t =
	    PlayerHandler.invoke t in
	  let _ =
	    broadcast t @@ `Message( t#name, `GameStart) in
	    send ch @@ `Ok id;
	    t
	end else begin
	  send ch @@ `Ok id;
	  t
	end
  else begin
    send ch @@ `Error (id, "not join player");
    t
  end

let handle t ch pid = function
  | `Message msg ->
      let open Maybe in
	(ignore @@ perform begin
	   name <-- lookup pid t#names;
	   let response =
	     `Message (t#name, `Player (name, msg)) in
	   return @@
	     List.iter (fun (_,ch')-> send ch' response) t#clients
	 end);
	t
  | `Query (id, `Join name) ->
      send ch @@ `Ok id;
      let t =
	t#clients <- (name, ch) :: t#clients in
	t#names <- (pid, name) :: t#names
  | `Query (id, `Ready) ->
      ready t ch pid id
  | `Query (id, (#PlayerHandler.request as req)) ->
      begin match PlayerHandler.handle t ch req with
	  Left t ->
	    send ch @@ `Ok id;
	    t
	| Right msg ->
	    send ch @@ `Error (id,msg);
	    t
      end
  | `Query (id, `List `Mine) ->
      let open Game in
	send ch @@ `Cards (id, (me t#game).hands);
	t
  | `Query (id, `List `Supply) ->
      let open Game in
	send ch @@ `Cards (id, t#game.board.supply);
	t

let initial name = {|
    name     = name;
    fiber    = None;
    observer = Observer.make ();
    game     = Game.make [] [];
    clients  = [];
    ready    = [];
    names    = []
|}

let create name =
  let ch =
    Event.new_channel () in
  let _ =
    state_daemon (initial name) ~f:begin fun state ->
      let (peer, src, req) =
	Event.sync @@ Event.receive ch in
	handle state src peer req
    end in
    ch

let handle t x y z =
  Event.send t (x, y, z)
