open Base
open ThreadUtils
open ListUtil


type t = (Protocol.response Event.channel * Protocol.game_request) Event.channel

let send ch e =
  Event.sync @@ Event.send ch e

let game t =
  let players =
    ListLabels.map t#clients ~f:begin fun (_,name)->
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

let ready t ch id =
  if List.mem ch @@ List.map fst t#clients then
    if List.mem ch t#ready then
      (send ch @@ `Error (id, "already ready");
       t)
    else
      let t =
	t#ready <- ch :: t#ready in
	if List.length t#ready = List.length t#clients then begin
	  let t =
	    t#game <- game t in
	  let t =
	    PlayerHandler.invoke t in
	    send ch @@ `Ok id;
	    t
	end else begin
	  send ch @@ `Ok id;
	  t
	end
  else
    (send ch @@ `Error (id, "not join player");
     t)

let handle t ch = function
  | `Message msg ->
      let open Maybe in
	(ignore @@ perform begin
	   name <-- lookup ch t#clients;
	   let response =
	     `Message (t#name, name, msg) in
	   return @@
	     List.iter (fun (ch',_)-> send ch' response) t#clients
	 end);
	t
  | `Query (id, `Join name) ->
      send ch @@ `Ok id;
      t#clients <- (ch, name) :: t#clients
  | `Query (id, `Ready) ->
      ready t ch id
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
    ready    = []
|}

let create name =
  let ch =
    Event.new_channel () in
  let _ =
    state_daemon (initial name) ~f:begin fun state ->
      let (src, req) =
	Event.sync @@ Event.receive ch in
	handle state src req
    end in
    ch

let handle =
  uncurry $ Event.send
