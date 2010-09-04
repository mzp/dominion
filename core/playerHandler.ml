open Base
open Cc
open ListUtil
open Game
open HandlerBase

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type request = [
  | `Select of Game.card
  | `Skip
  ]
  type state = S.t HandlerBase.state

  let table : (S.t, request, state) ContHandler.t =
    ContHandler.make ()

  let handle =
    ContHandler.resume table

  type client = {
    client : S.t;
    suspend: (S.t, request, state) ContHandler.suspend
  }

  (*
    ユーザとのインタラクション。
    predを満すリクエストを受け取ると、処理を継続する。
  *)
  let user name ~p { suspend; _ } state =
    let client =
      fst @@ List.find (fun (_,y)-> y = name) state.clients in
      suspend client p state

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

  let me state =
    (current_player state).name

  let players state =
    List.map (fun { name; _ } -> name) state.game.players

  let others state =
    players state -- [ me state ]

  let find_player x state =
    List.find (fun {name; _ } -> name = x) state.game.players

  let update_game ~f state =
    { state with game = f state.game }

  let update_player name ~f state =
    update_game state ~f:(Game.update_player name ~f)

  let update_board ~f state =
    update_game state ~f:(Game.update_board ~f)

  let update ~f kind state =
    match kind with
	`Hands name ->
	  update_player name state ~f:(fun me -> { me with hands = f me.hands } )
      | `Decks name ->
	  update_player name state ~f:(fun me -> { me with decks = f me.decks } )
      | `Discards name ->
	  update_player name state ~f:(fun me -> { me with discards = f me.discards } )
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
  let action n name state =
    update_player name state ~f:(fun me -> { me with action = me.action + n })

  let buy n name state =
    update_player name state ~f:(fun me -> { me with buy = me.buy + n })

  let coin n name state =
    update_player name state ~f:(fun me -> { me with coin = me.coin + n })

  let draw n name =
    update_player name ~f:begin fun me ->
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
  let in_hands state c =
    List.mem c (current_player state).hands

  let in_supply state c =
    List.mem c state.game.board.supply

  (* カードの選択 *)
  let select_card name client state ~p =
    until state ~f:begin fun state ->
      perform begin
	(request, state) <-- user name client state ~p:(skip <||> select);
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
    let me =
      me state in
    many_ state ~f:begin fun state ->
      if pred @@ current_player state then
	return None
      else
	perform begin
	  (request, state) <-- select_card me client state ~p:(p state);
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

  let rec fold_m ~f a = function
    | [] ->
	return a
    | x::xs ->
	perform (y <-- f a x;
		 fold_m ~f y xs)

  (* Actionカードの定義 *)
  let card_action kind client state=
    let me =
      me state in
    match kind with
      | `Cellar ->
	  perform begin
	    (xs,state) <-- many state ~f:begin fun state ->
	      perform begin
		let _ = notify client "select discard card" in
		  (request, state) <-- select_card me client state ~p:(in_hands state);
		  match request with
		    | `Card c ->
			return (Some c, state)
		    | `Skip ->
			return (None, state)
	      end
	    end;
	    let n = List.length xs in
	      return @@ state
	      +> move (`Hands me) (`Discards me) xs
	      +> draw n me
	  end
      | `Market ->
	  return @@ state
	  +> action 1 me
	  +> buy    1 me
	  +> coin   1 me
	  +> draw 1 me
      | `Mine ->
	  perform begin
	    (r,state) <-- select_card me client state ~p:(is_treasure <&&> in_hands state);
	    match r with
		`Card c1 ->
		  perform begin
		    (r,state) <-- select_card me client state ~p:(is_treasure <&&>
								 in_supply state <&&>
								 (fun c -> Game.cost c <= Game.cost c1 + 3));
		    begin match r with
			`Card c2 ->
			  return @@ state
			  +> move (`Hands me)  `Trash  [c1]
			  +> move `Supply (`Hands me) [c2]
		      | `Skip ->
			  return state
		    end
		  end
	      | `Skip ->
		  return state
	  end
      | `Remodel ->
	  perform begin
	    (r,state) <-- select_card me client state ~p:(in_hands state);
	    match r with
		`Card c1 ->
		  perform begin
		    (r,state) <-- select_card me client state ~p:(in_supply state <&&>
								 (fun c -> Game.cost c <= Game.cost c1 + 2));
		    begin match r with
			`Card c2 ->
			  return @@ state
			  +> move (`Hands  me) `Trash  [c1]
			  +> move `Supply (`Hands me) [c2]
		      | `Skip ->
			  return state
		    end
		  end
	      | `Skip ->
		  return state
	  end
      | `Smithy ->
	  state
	  +> draw 3 me
	  +> return
      | `Village ->
	  state
	  +> action 2 me
	  +> draw 1 me
	  +> return
      | `Woodcutter ->
	  state
	  +> buy 1 me
	  +> coin 2 me
	  +> return
      | `Workshop ->
	  perform begin
	    (r, state) <-- select_card me client state
	      ~p:(in_supply state <&&> (fun c -> Game.cost c <= 4));
	    match r with
		`Card c ->
		  state
		  +> move `Supply (`Hands me) [ c ]
		  +> return
	      | `Skip ->
		  return state
	  end
      | `Moat ->
	  return @@ draw 2 me state
      | `Militia ->
	  let attack name state =
	    perform ((_,state) <-- until state ~f:begin fun state ->
		       let { hands; _ } =
			 find_player name state in
			 if List.length hands <= 3 then
			   return (Some (),state)
			 else
			   perform begin
			     (r, state) <-- select_card name client state
			       ~p:(fun c -> List.mem c hands);
			     match r with
			       | `Skip -> return (None,state)
			       | `Card c ->
				   let state =
				     move (`Hands name) (`Discards name)
				       [ c ] state in
				     return (None,state)
			   end
		     end;
		     return state)
	  in
	    fold_m state (others state) ~f:begin fun state name ->
	      let { hands; _ } =
		find_player name state in
		if List.exists is_reaction hands then
		  perform begin
		    (r, state) <-- select_card name client state
		      ~p:(fun c -> List.mem c hands && c = `Moat);
		    match r with
		      | `Card `Moat ->
			  return state
		      | `Skip ->
			  attack name state
		      | `Card _ ->
			  failwith "must not happen"
		  end
		else
		  attack name state
	    end
      | _ ->
	  failwith "not action card"

  (* actionフェーズ *)
  let action_phase client state =
    let me = me state in
      phase client state (fun { action; _ } -> action = 0)
	~p:(fun state -> in_hands state <&&> is_action)
	~f:begin fun state c ->
	  let state =
	    state
	    +> move (`Hands me) `PlayArea [c] in
	    perform begin
	      state <-- (card_action c) client state;
	      let state = update_player me state ~f:(fun me -> {me with action = me.action -1 }) in
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
	      in_supply state c && cost c < cap)
      ~f:begin fun state c ->
	let me = me state in
	return @@ `Val (state
			+> buy  (- 1) me
			+> coin (- (cost c)) me
			+> move `Supply (`Discards me) [c])
      end

  let wrap state cc =
    let open Cc in
      perform begin
	r <-- cc;
	match r with
	    Left (_, game) ->
	      Cc.return { state with game }
	  | Right _ ->
	      failwith ""
      end

  (* cleanupフェーズ *)
  let cleanup_phase _ ({ game; _ } as state) =
    let open Rule in
    let me =
      me state in
      wrap state @@ Rule.run game ~f:perform begin
	move (`Hands me) (`Discards me) (current_player state).hands;
	draw me 5;
	action me @@ const 1;
	buy    me @@ const 1;
	coin   me @@ const 0
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
	  state <-- action_phase client state;
	  (* buy phase *)
	  let _ = send_all state @@ `Phase (`Buy, name) in
	    state <-- buy_phase client state;
	    (* cleanup phase *)
	    let _ = send_all state @@ `Phase (`Cleanup, name) in
	      state <-- cleanup_phase client state;
	      let state = update_game state
		~f:(fun g ->
		      { g with me = (g.me + 1) mod List.length g.players}) in
		ContHandler.end_ state
      end

  let invoke state =
    let client =
      current_client state in
      ignore @@ ContHandler.start table
	~f:(fun suspend ->  turn { suspend; client })
	state

  (* for test *)
  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients= HList.zip
	xs
	(List.map (fun {name; _ } -> name) g.players);
      ready=xs;
      playing =true }
end
