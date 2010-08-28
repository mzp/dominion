open Base
open Cc
open ListUtil

module Make(S : Protocol.Rpc)(B : HandlerBase.S with type t = S.t)  = struct
  open B

  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state = B.state

  type cc = [
    `Cc of state * (request -> bool) * (request -> state -> (unit, cc) Cc.CONT.mc)
  | `End of state
  ]

  type client = {
    client : S.t;
    prompt : cc Cc.prompt
  }


  type action = ((request -> bool) * (request -> state -> (unit, cc) Cc.CONT.mc))

  let current_player s =
    Game.me s.game

  (*
    継続サーバフレームワーク
  *)
  let table : (S.t, action) Hashtbl.t =
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

  let run f state =
    let open Cc in
    let client =
      B.current_client state in
      ignore @@ save_cc client @@ perform begin
	p <-- new_prompt ();
	pushP p @@ f { prompt = p; client } state
      end

  let handle client request state  =
    let open Cc in
      if Hashtbl.mem table client then
	let (p, k) =
	  Hashtbl.find table client in
	  if p request then
	    Left (save_cc client @@ k request state)
	  else begin
	    Right "invalid request";
	  end
      else begin
	Right "invalid request";
      end

  let user { prompt; _ } state pred =
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

  let send { client; _ } e =
    S.send client e

  let sum xs =
    List.fold_left (+) 0 xs

  let phase client state pred ~f =
    let open Cc in
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

  let rec many client state pred =
    let open Cc in
      perform begin
	(request, state) <-- user client state (skip <> select);
	match request with
	  | `Skip ->
	      return ([], state)
	  | `Select c ->
	      if pred c then
		perform ((xs,state) <-- many client state pred;
			 return ((c :: xs), state))
	      else
		return ([], state)
      end

  let card_action =
    let open Cc in
    let open Game in
      function
	| `Cellar -> begin fun client state ->
	    let me =
	      current_player state in
	      perform begin
		let _ = send client @@ `Notify "select discard card" in
		(xs,state) <-- many client state (fun c -> List.mem c me.hands);
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
    let open Cc in
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
	state <-- action client state;
	(* buy phase *)
	let _ = send_all state @@ `Phase (`Buy, name) in
	state <-- buy client state;
	(* cleanup phase *)
	let _ = send_all state @@ `Phase (`Cleanup, name) in
	state <-- cleanup client state;
	return @@ `End state
      end

  let invoke =
    run turn

  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end
