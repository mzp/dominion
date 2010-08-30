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
  let bin_op op f g x =
    op (f x) (g x)

  let (<||>) = bin_op (||)
  let (<&&>) = bin_op (&&)

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


  (* fがSomeを返すまで、fを繰替えす。*)
  let rec until ~f state =
    perform begin
      (result,state) <-- f state;
      match result with
	| None ->
	    until ~f state
	| Some c ->
	    return (c,state)
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

  (* カードの移動 *)
  let move src dest xs state =
    state
    +> update src  ~f:(fun ys -> ys -- xs)
    +> update dest ~f:(fun ys -> xs @  ys)

  (* +n action/buy/coin/draw *)
  let action n state =
    update_player state ~f:(fun me -> { me with action = me.action + n })

  let buy n state =
    update_player state ~f:(fun me -> { me with buy = me.buy + n })

  let coin n state =
    update_player state ~f:(fun me -> { me with coin = me.coin + n })

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

  (* カード選択の述語 *)
  let current_player s =
    Game.me s.game

  let is_hands state c =
    List.mem c (current_player state).hands

  let is_supply state c =
    List.mem c state.game.board.supply

  (* カードの選択 *)
  let select_card client state ~p =
    until state ~f:begin fun state ->
      perform begin
	(request, state) <-- user client state (skip <||> select);
	match request with
	  | `Skip ->
	      return (Some `Skip, state)
	  | `Select c ->
	      if p c then
		return (Some (`Card c), state)
	      else
		return (None, state)
      end
    end

  let phase client state pred ~p ~f =
    many_ state ~f:begin fun state ->
      if pred @@ current_player state then
	return None
      else
	perform begin
	  (request, state) <-- select_card client state ~p:(p state);
	  match request with
	    | `Skip ->
		return None
	    | `Card c ->
		perform (r <-- f state c;
			 match r with
			     `Val state ->
			       ok client;
			       return @@ Some state
			   | `Err msg ->
			       error client msg;
			       return @@ Some state)
	end
    end

  (* Actionカードの定義 *)
  let card_action kind client state=
    let me =
      current_player state in
      match kind with
	| `Cellar ->
	    perform begin
	      (xs,state) <-- many state ~f:begin fun state ->
		perform begin
		  let _ = notify client "select discard card" in
		    (request, state) <-- user client state (skip <||> select);
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
	| `Market ->
	    return @@ state
	      +> action 1
	      +> buy    1
	      +> coin   1
	      +> draw 1
	| `Mine ->
	    perform begin
	      (r,state) <-- select_card client state ~p:(is_treasure <&&> is_hands state);
	      match r with
		  `Card c1 ->
		    perform begin
		      (r,state) <-- select_card client state ~p:(is_treasure <&&>
								   is_supply state <&&>
								   (fun c -> Game.cost c <= Game.cost c1 + 3));
		      begin match r with
			  `Card c2 ->
			    return @@ state
			    +> move `Hands  `Trash  [c1]
			    +> move `Supply `Hands [c2]
			| `Skip ->
			    return state
		      end
		    end
		| `Skip ->
		    return state
	    end
	| _ ->
	    failwith "not action card"

  (* actionフェーズ *)
  let action_phase client state =
    phase client state (fun { action; _ } -> action = 0)
      ~p:(fun state -> is_hands state <&&> is_action)
      ~f:begin fun state c ->
	let state =
	  state
	  +> move `Hands `PlayArea [c] in
	  perform begin
	    state <-- (card_action c) client state;
	    let state = update_player state ~f:(fun me -> {me with action = me.action -1 }) in
	      return @@ `Val state
	  end
      end

  (* buyフェーズ *)
  let buy_phase client state =
    phase client state (fun { buy; _ } -> buy = 0)
      ~p:(fun state c ->
	    let me =
	      current_player state in
	    let sum xs =
	      List.fold_left (+) 0 xs in
	    let cap =
	      me.coin + sum (List.map Game.coin me.hands) in
	      is_supply state c && cost c < cap)
      ~f:begin fun state c ->
	return @@ `Val (state
			+> buy  (- 1)
			+> coin (- (cost c))
			+> move `Supply `Discards [c])
      end

  (* cleanupフェーズ *)
  let cleanup_phase _ state =
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
	  state <-- action_phase client state;
	  (* buy phase *)
	  let _ = send_all state @@ `Phase (`Buy, name) in
	    state <-- buy_phase client state;
	    (* cleanup phase *)
	    let _ = send_all state @@ `Phase (`Cleanup, name) in
	      state <-- cleanup_phase client state;
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
