open Base
open ThreadUtils
open ListUtil
open Ccell

type ('a,'b) request = {
  pid     : 'a;
  return   : Protocol.return Ivar.t;
  notify   : Protocol.notify Mbox.t;
  request  : 'b;
}

type 'a t = ('a, Protocol.request) request Event.channel

(** {3 ゲームごとのスレッド }*)

(** ゲームの状態を保持するオブジェクト *)
let initial name = {|
    name     = name;
    fiber    = None;
    observer = Observer.make ();
    game     = Game.make [] [];
    clients  = [];
    ready    = [];
    names    = []
|}

(** ゲームの初期配置を構築する *)
let make_game t =
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

(** readyの処理が複雑なので別関数にした *)
let on_ready t id {pid; return; _}=
  if lookup pid t#names <> None then
    if List.mem pid t#ready then begin
      Ivar.put return @@ `Error (id, "already ready");
      t
    end else
      let t =
	t#ready <- pid :: t#ready in
	if List.length t#ready = List.length t#names then begin
	  let t =
	    t#game <- make_game t in
	  let t =
	    PlayerHandler.invoke t in
	  let _ =
	    Observer.__fire t#observer `GameStart in
	    Ivar.put return @@ `Ok id;
	    t
	end else begin
	  Ivar.put return @@ `Ok id;
	  t
	end
  else begin
    Ivar.put return @@ `Error (id, "not join player");
    t
  end

let game_thread t ({ return; pid; _ } as r) =
  match r.request with
    | `Message text ->
	begin match lookup pid t#names with
	    Some name ->
	      Observer.__fire t#observer @@ `Player (name, text);
	  | None ->
	      ()
	end;
	t
    | `Query (id, `Join name) ->
	Ivar.put return @@ `Ok id;
	let t =
	  t#clients <- (name, pid) :: t#clients in
	let _ =
	  Observer.listen t#observer
	    (fun c -> Event.sync @@ Mbox.push r.notify @@ `Message(t#name, c)) in
	  t#names <- (pid, name) :: t#names
    | `Query (id, `Ready) ->
	on_ready t id r
    | `Query (id, (#PlayerHandler.request as req)) ->
	begin match PlayerHandler.handle t pid req with
	    Left t ->
	      Ivar.put return @@ `Ok id;
	      t
	  | Right msg ->
	      Ivar.put return @@ `Error (id,msg);
	      t
	end
    | `Query (id, `List `Mine) ->
	let open Game in
	  Ivar.put return @@ `Cards (id, (me t#game).hands);
	  t
    | `Query (id, `List `Supply) ->
	let open Game in
	  Ivar.put return @@ `Cards (id, t#game.board.supply);
	  t

let create name =
  with_channel begin fun ch ->
    state_daemon (initial name) ~f:begin fun state ->
      game_thread state (Event.sync @@ Event.receive ch)
    end
  end


(** {3 システム全体の制御 }*)

(** システムを制御するスレッド *)
let system_thread ch games =
  let r =
    Event.sync @@ Event.receive ch in
    Logger.debug "accept request" ();
    match r.request with
      | `List id ->
	  Logger.debug "sending game rooms: %s" (Std.dump @@ List.map fst games) ();
	  Ivar.put r.return @@ `Games(id, List.map fst games);
	  games
      | `Make (id,name) ->
	  Logger.debug "make room: %s" name ();
	  Ivar.put r.return @@ `Ok id;
	  (name, create name) :: games
      | `Game(name, request) ->
	  let open Maybe in
	    ignore (perform begin
		      t <-- lookup name games;
		      return @@ Event.sync @@ Event.send t {
			pid   = r.pid;
			return = r.return;
			notify = r.notify;
			request
		      }
		    end);
	    games

let start () : 'a t =
  with_channel begin fun ch ->
    state_daemon ~f:(system_thread ch) []
  end
