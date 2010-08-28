open Base
open Cc
open ListUtil

module Make(S : Protocol.Rpc)(B : HandlerBase.S with type t = S.t)  = struct
  open B
  type req = [
  | `Select of Game.card
  | `Skip
  ]
  type request = req

  module Cont = ContHandler.Make(struct
				   type client  = S.t
				   type request = req

				   type state = B.state
				 end)
  let handle = Cont.handle

  type client = {
    client : S.t;
    prompt : Cont.cc Cc.prompt
  }

  (*
    ユーザとのインタラクション。
    predを満すリクエストを受け取ると、処理を継続する。
  *)
  let user { prompt; _ } state pred =
    let open Cc in
    let handle k request state =
      k @@ return (request, state) in
      shiftP prompt (fun k -> return @@ `Cc(state, (pred , handle k)))

  (* skipリクエストならtrueを返す *)
  let skip =  function
	`Skip -> true
      | _     -> false

  (* selectリクエストならtrueを返す *)
  let select = function
      `Select _ -> true
    | _         -> false

  (* 述語の合成 *)
  let (<>) f g x =
    f x || g x

  (* fがNoneを返すまで、fを繰替えす *)
  let rec many ~f state =
    perform begin
      (result, state) <-- f state;
      match result with
	| None ->
	    return ([], state)
	| Some c ->
	    perform ((xs,state) <-- many ~f state;
		     return ((c :: xs), state))
    end

  (* pがtrueを返すまで、fを繰替えす *)
  let rec until ~p ~f state =
    if p state then
      return state
    else
      perform begin
	state <-- f;
	until ~p ~f state
      end

  let update_game ~f state =
    { state with
	game = f state.game }

  let update_player ~f state =
    update_game state ~f:(fun g -> Game.update ~f g)

  let update_board ~f state =
    update_game
      state
      ~f:(fun g ->
	    { g with Game.board = f g.Game.board })

  let send { client; _ } e =
    S.send client e

  let sum xs =
    List.fold_left (+) 0 xs

  let current_player s =
    Game.me s.game

  let phase client state pred ~f =
    let rec until state =
      let me =
	current_player state in
	if pred me then
	  return state
	else
	  perform begin
	    (request, state) <-- user client state (skip <> select);
	    match request with
	    | `Skip ->
		return state
	    | `Select c ->
		perform (r <-- f c state;
			 until @@ match r with
			     `Val state' ->
			       send client `Ok;
			       state'
			   | `Err msg ->
			       send client @@ `Error msg;
			       state)
	  end in
      until state


  (* game definiton *)
  let card_action =
    let open Game in
      function
	| `Cellar -> begin fun client state ->
	    let me =
	      current_player state in
	      perform begin
		let _ = send client @@ `Notify "select discard card" in
		(xs,state) <-- many state ~f:begin fun state ->
		  perform begin
		    (request, state) <-- user client state (skip <> select);
		    match request with
		      | `Select c when List.mem c me.hands ->
			  return (Some c, state)
		      | `Skip | `Select _ ->
			  return (None, state)
		  end
		end;
		let n = List.length xs in
		return @@
		  state
		  +> update_player ~f:(fun me ->
				      {	me with
					  hands = (HList.take n me.decks) @ (me.hands -- xs);
					  discards = xs @ me.discards;
					  decks  = HList.drop n me.decks
				      })
	      end
	  end
	| _ ->
	    failwith "not action card"

  let action client state =
    let open Game in
      phase client state (fun { action; _ } -> action = 0) ~f:begin fun c state ->
	let me =
	  current_player state in
	  if List.mem c me.hands && Game.is_action c then
	    let state =
	      state
	      +> update_board  ~f:(fun b -> { b with play_area = c :: b.play_area} )
	      +> update_player ~f:(fun p -> { p with hands     = p.hands -- [ c ] } ) in
	      perform begin
		state <-- (card_action c) client state;
		let state = update_player state ~f:(fun me -> {me with action = me.action -1 }) in
		return @@ `Val state
	      end
	  else
	    return (`Err "not have the card")
      end

  let buy client state =
    let open Game in
      phase client state (fun { buy; _ } -> buy = 0) ~f:begin fun c state ->
	let me =
	  current_player state in
	let cap =
	  me.coin + sum (List.map coin me.hands) in
	  if List.mem c state.game.board.supply && cost c < cap then
	    Cc.return (`Val (update_player state
			       ~f:(fun me ->
				     { me with
					 buy  = me.buy - 1;
					 coin = me.coin - cost c;
					 discards = c :: me.discards })))
	  else
	    Cc.return @@ `Err "not enough coin"
      end

  let cleanup _ state =
    let open Game in
    let n = 5 in
      Cc.return @@ update_player state ~f:begin fun player ->
	let discards' =
	  player.hands @ player.discards in
	let len =
	  List.length player.decks in
	  if len >= n then
	    { player with
		discards = discards';
		hands    = HList.take n player.decks;
		decks    = HList.drop n player.decks;
		action   = 1;
		buy      = 1;
		coin     = 1;
	    }
	  else
	    let decks' =
	      shuffle discards' in
	      { player with
		  discards = [];
		  hands    = player.decks @ HList.take (n - len) decks';
		  decks    = HList.drop (n - len) decks';
		  action   = 1;
		  buy      = 1;
		  coin     = 1;
	      }
      end

  let turn client state =
    let log name cs =
      Logger.debug "%s: %s" name (Std.dump (List.map Game.to_string cs)) () in
    let me =
      current_player state in
    let name =
      me.Game.name in
      perform begin
	let _ =
	  Logger.debug "player : %s" name ();
	  log "decks" me.Game.decks;
	  log "hands" me.Game.hands;
	  log "discards" me.Game.discards in
	let _ = send_all state @@ `Turn name in
	(* action phase *)
	let _ = send_all state @@ `Phase (`Action, name) in
	state <-- action client state;
	(* buy phase *)
	let _ = send_all state @@ `Phase (`Buy, name) in
	state <-- buy client state;
	(* cleanup phase *)
	let _ = send_all state @@ `Phase (`Cleanup, name) in
	state <-- cleanup client state;
	return @@ `End state
      end

  let invoke state =
    let client =
      List.nth state.ready state.game.Game.me in
      Cont.run
	(fun p ->  turn { prompt = p; client })
	client
	state

  (* for test *)
  type state = B.state
  type cc = Cont.cc
  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end
