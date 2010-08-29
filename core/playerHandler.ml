open Base
open Cc
open ListUtil
open Game
open HandlerBase

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type req = [
  | `Select of Game.card
  | `Skip
  ]
  type request = req
  type state = S.t HandlerBase.state

  module Cont = ContHandler.Make(struct
				   type client  = S.t
				   type request = req

				   type state = S.t HandlerBase.state
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
    let handle k request state =
      k @@ return (request, state) in
      shiftP prompt (fun k -> return @@ `Cc(state, (pred , handle k)))


  (* ユーザに情報を送る *)
  let send { client; _ } e =
    S.send client e

  let notify { client; _ } s =
    S.send client @@ `Notify s

  let ok { client; _ } =
    S.send client `Ok

  let error { client; _ } s =
    S.send client @@  `Error s

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

  (*
    fがNoneを返すまで、fを繰替えす。
    fの返した値をリストにして返す
  *)
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

  (* fがNoneを返すまで、fを繰替えす。 *)
  let rec many_ ~f state =
    perform begin
      result <-- f state;
      match result with
	| None ->
	    return state
	| Some state ->
	    many_ ~f state
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

  let update ~f kind state =
    match kind with
	`Hands ->
	  update_player state ~f:(fun me -> { me with hands = f me.hands } )
      | `Decks ->
	  update_player state ~f:(fun me -> { me with decks = f me.decks } )
      | `Discards ->
	  update_player state ~f:(fun me -> { me with discards = f me.discards } )
      | `PlayArea ->
	  update_board state ~f:(fun b -> { b with play_area = f b.play_area })
      | `Supply ->
	  update_board state ~f:(fun b -> { b with supply = f b.supply })
      | `Trash ->
	  update_board state ~f:(fun b -> { b with trash = f b.trash })

  let move src dest xs state =
    state
    +> update src  ~f:(fun ys -> ys -- xs)
    +> update dest ~f:(fun ys -> xs @  ys)

  let draw n =
    update_player ~f:begin fun me ->
      let len =
	List.length me.decks in
	if len >= n then
	  { me with
	      hands    = HList.take n me.decks @ me.hands;
	      decks    = HList.drop n me.decks }
	else
	  let decks' =
	    shuffle me.discards in
	    { me with
		discards = [];
		hands    = me.decks @ HList.take (n - len) decks';
		decks    = HList.drop (n - len) decks';
	    }
    end

  let sum xs =
    List.fold_left (+) 0 xs

  let current_player s =
    Game.me s.game

  let phase client state pred ~f =
    many_ state ~f:begin fun state ->
      if pred @@ current_player state then
	return None
      else
	perform begin
	  (request, state) <-- user client state (skip <> select);
	  match request with
	    | `Skip ->
		return None
	    | `Select c ->
		perform (r <-- f c state;
			 match r with
			     `Val state ->
			       ok client;
			       return @@ Some state
			   | `Err msg ->
			       error client msg;
			       return @@ Some state)
	end
    end

  (* game definiton *)
  let card_action kind client state=
    let me =
      current_player state in
      match kind with
	| `Cellar ->
	    perform begin
	      (xs,state) <-- many state ~f:begin fun state ->
		perform begin
		  let _ = notify client "select discard card" in
		    (request, state) <-- user client state (skip <> select);
		    match request with
		      | `Select c when List.mem c me.hands ->
			  return (Some c, state)
		      | `Skip | `Select _ ->
			  return (None, state)
		end
	      end;
	      let n = List.length xs in
	      return @@ state
		+> move `Hands `Discards xs
		+> draw n
	    end
	| _ ->
	    failwith "not action card"

  let action client state =
    phase client state (fun { action; _ } -> action = 0) ~f:begin fun c state ->
      let me =
	current_player state in
	if List.mem c me.hands && Game.is_action c then
	  let state =
	    state
	    +> move `Hands `PlayArea [c] in
	    perform begin
	      state <-- (card_action c) client state;
	      let state = update_player state ~f:(fun me -> {me with action = me.action -1 }) in
		return @@ `Val state
	    end
	else
	  return (`Err "not have the card")
    end

  let buy client state =
    phase client state (fun { buy; _ } -> buy = 0) ~f:begin fun c state ->
      let me =
	current_player state in
      let cap =
	me.coin + sum (List.map coin me.hands) in
	if List.mem c state.game.board.supply && cost c < cap then
	  let state =
	    state
	    +> update_player~f:(fun me ->
				  { me with
				      buy  = me.buy - 1;
				      coin = me.coin - cost c })
	    +> move `Supply `Discards [c]
	  in
	    return (`Val state)
	else
	  return @@ `Err "not enough coin"
    end

  let cleanup _ state =
    let state =
      state
      +> move `Hands `Discards (current_player state).hands
      +> draw 5
      +> update_player ~f:(fun me -> { me with action=1; buy=1;coin=0}) in
      return @@ state

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
  type cc = Cont.cc
  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end
