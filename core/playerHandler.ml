open Base

module Make(S : Protocol.Rpc)(B : HandlerBase.S with type t = S.t)  = struct
  open B
  open ListUtil
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  let current_player s =
    Game.me s.game

  let table : (S.t, ((request -> bool) *
                       (request -> state ->
			  (unit, [ `Cc of state * (request -> bool) * 'b | `End of state ])
			    Cc.CONT.mc as 'b))) Hashtbl.t =
    Hashtbl.create 0

  let save_cc client cont =
    match Cc.run cont with
	`End ({game; _ } as state) ->
	  let open Game in
	  let me =
	    (game.me + 1) mod (List.length state.ready) in
	    Hashtbl.clear table;
	    { state with
		game = { game with me }}
      | `Cc (state, pred, cc) ->
	  Hashtbl.add table client (pred, cc);
	  state

  let user prompt state pred =
    let open Cc in
    let handle k request state =
      k @@ return (request, state) in
      shiftP prompt (fun k -> return @@ `Cc(state, pred , handle k))

  let skip =  function
	`Skip ->
	  true
      | _ ->
	  false

  let select = function
      `Select _ ->
	true
    | _ ->
	false

  let (<>) f g x =
    f x || g x

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

  let sum xs =
    List.fold_left (+) 0 xs

  let phase p client state pred ~f =
    let open Cc in
    let rec until state =
      let me =
	current_player state in
	if pred me then
	  return state
	else
	  perform begin
	    (request, state) <-- user p state (skip <> select);
	    match request with
	    | `Skip ->
		return state
	    | `Select c ->
		perform (r <-- f c state;
			 until @@ match r with
			     `Val state' ->
			       S.send client `Ok;
			       state'
			   | `Err msg ->
			       S.send client @@ `Error msg;
			       state)
	  end in
      until state

  let rec repeat p client state n pred =
    let open Cc in
      if n = 0 then
	return ([],state)
      else
	perform begin
	  (request, state) <-- user p state (skip <> select);
	  match request with
	    | `Skip ->
		return ([], state)
	    | `Select c ->
		if pred c then
		  perform ((xs,state) <-- repeat p client state (n-1) pred;
			   return ((c :: xs), state))
		else
		  return ([], state)
	end



  let rec many p client state pred =
    let open Cc in
      perform begin
	(request, state) <-- user p state (skip <> select);
	match request with
	  | `Skip ->
	      return ([], state)
	  | `Select c ->
	      if pred c then
		perform ((xs,state) <-- many p client state pred;
			 return ((c :: xs), state))
	      else
		return ([], state)
      end

  let card_action =
    let open Cc in
    let open Game in
      function
	| `Cellar -> begin fun p client state ->
	    let me =
	      current_player state in
	      perform begin
		let _ = S.send client @@ `Notify "select discard card" in
		(xs,state) <-- many p client state (fun c -> List.mem c me.hands);
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

  let action p client state =
    let open Game in
    let open Cc in
      phase p client state (fun { action; _ } -> action = 0) ~f:begin fun c state ->
	let me =
	  current_player state in
	  if List.mem c me.hands && Game.is_action c then
	    let state =
	      state
	      +> update_board  ~f:(fun b -> { b with play_area = c :: b.play_area} )
	      +> update_player ~f:(fun p -> { p with hands     = p.hands -- [ c ] } ) in
	      perform begin
		state <-- (card_action c) p client state;
		let state = update_player state ~f:(fun me -> {me with action = me.action -1 }) in
		return @@ `Val state
	      end
	  else
	    return (`Err "not have the card")
      end

  let buy p client state =
    let open Game in
      phase p client state (fun { buy; _ } -> buy = 0) ~f:begin fun c state ->
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

  let cleanup _p _client state =
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

  let turn p client state =
    let open Cc in
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
	state <-- action p client state;
	(* buy phase *)
	let _ = send_all state @@ `Phase (`Buy, name) in
	state <-- buy p client state;
	(* cleanup phase *)
	let _ = send_all state @@ `Phase (`Cleanup, name) in
	state <-- cleanup p client state;
	return @@ `End state
      end

  let invoke state =
    let open Cc in
    let client =
      B.current_client state in
      save_cc client @@ perform begin
	p <-- new_prompt ();
	pushP p @@ turn p client state
      end

  type state = B.state
  let handle client request state  =
    let open Cc in
      if Hashtbl.mem table client then
	let (p, k) =
	  Hashtbl.find table client in
	  if p request then
	    save_cc client @@ k request state
	  else begin
	    Logger.error "unexpected request" ();
	    S.send client @@ `Error "invalid request";
	    state
	  end
      else begin
	Logger.error "no cc" ();
	S.send client @@ `Error "invalid request";
	state
      end

  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end
