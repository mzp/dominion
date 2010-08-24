open Base

module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make(S : S) = struct
  type t = {
    clients : (S.t * string) list;
    ready   : S.t list;
    playing : bool;
    game : Game.t
  }

  let initial = {
    clients = [];
    ready   = [];
    playing = false;
    game    = Game.make [] []
  }

  let find p xs =
    (option (List.find p)) xs

  let player_of_client client s =
    Maybe.(perform begin
	     name <-- lookup client s.clients;
	     Game.(find (fun p -> p.name = name) s.game.players)
	   end)

  let current_player s =
    let open Game in
    List.nth s.game.players s.game.me

  let current_client s =
    let open Game in
      fst @@ List.nth s.clients s.game.me

  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)

  let add x xs =
    if List.mem x xs then
      xs
    else
      x :: xs

  let shuffle xs =
    Random.self_init ();
    List.map (fun x -> (Random.int (List.length xs), x)) xs
    +> List.sort (fun (x,_) (y,_) -> compare x y)
    +> List.map snd

  type common_request = [
  | `Join of string
  | `Query of [`Supply | `Mine ]
  | `Say of string
  ]

  let handle_common client request ({ clients; game; _ } as state) =
    match request with
      | `Join name ->
	  S.send client `Ok;
	  { state with
	      clients = (client, name) :: clients }
      | `Say msg ->
	  ignore @@ Maybe.(perform begin
			     name <-- lookup client clients;
			     return @@ send_all state @@ `Chat (name, msg)
			   end);
	  state
      | `Query `Supply ->
	  S.send client @@ `Cards Game.(game.board.supply);
	  state
      | `Query `Mine ->
	  let open Game in
	    (S.send client @@
	       match player_of_client client state with
		   Some { hands; _ } ->
		     `Cards hands
		 | None ->
		     `Error "not join");
	    state

    let game { clients; _ } =
      let players =
	ListLabels.map clients ~f:begin fun (_,name)->
	  let init =
	    shuffle @@ List.concat [
	      HList.replicate 7 `Copper;
	      HList.replicate 3 `Estate;
	    ] in
	  let (hands, decks) =
	    HList.splitAt 5 init in
	    Game.make_player name ~hands ~decks
	end in
      let kindgdoms =
	HList.concat_map (HList.replicate 10) [
	  `Cellar;
	  `Market;
	  `Mine;
	  `Remodel;
	  `Smithy;
	  `Village;
	  `Woodcutter;
	  `Workshop;
	  `Militia;
	  `Moat;
	] in
      let treasures =
	HList.concat_map (HList.replicate 30) [
	  `Gold;
	  `Silver;
	  `Copper
	] in
      let victories =
	HList.concat_map (HList.replicate (if List.length players <= 2 then 8 else 12)) [
	  `Estate;
	  `Duchy;
	  `Province
	] in
      let cards =
	List.concat [ kindgdoms; treasures; victories ] in
	Game.make players cards

  let handle_ready client ({ clients; ready; _ } as state) =
    let state' =
      if List.exists (fun (c,_) -> c = client) clients then
	begin
	  S.send client @@ `Ok;
	  { state with ready = add client ready }
	end else begin
	  S.send client @@ `Error "error";
	  state
	end in
      if List.length state'.ready = List.length state'.clients then begin
	send_all state' `GameStart;
	{ state' with
	    playing = true;
	    game = game state' }
      end else
	state'

  type player_request = [
  | `Select of Game.card
  | `Skip
  ]

  let table : (S.t, ((player_request -> bool) *
                       (player_request -> t ->
			  (unit, [ `Cc of t * (player_request -> bool) * 'b | `End of t ])
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

  let update_player ~f state =
    { state with
	game = Game.update ~f state.game }

  let sum xs =
    List.fold_left (+) 0 xs

  let rec buy p client state =
    let open Cc in
    let open Game in
    let me =
      current_player state in
      if me.buy = 0 then
	return state
      else
	perform begin
	  (request, state) <-- user p state (skip <> select);
	  match request with
	    | `Skip ->
		return state
	    | `Select c ->
		let cap =
		  me.coin + sum (List.map coin me.hands) in
		  if List.mem c state.game.board.supply && coin c < cap then begin
		    S.send client @@ `Ok;
		    buy p client @@ update_player state ~f:begin fun player ->
		      { player with
			  buy  = player.buy - 1;
			  coin = player.coin - coin c;
			  discards = c :: player.discards }
		    end
		  end else begin
		    S.send client @@ `Error "not enough coin";
		    buy p client state
		  end
	end

  let cleanup n state =
    let open Game in
      Game.update state ~f:begin fun player ->
	let discards' =
	  player.hands @ player.discards in
	let len =
	  List.length player.decks in
	  if len >= n then
	    let _ =
	      Logger.debug "draw from decks" () in
	    { player with
		discards = discards';
		hands    = HList.take n player.decks;
		decks    = HList.drop n player.decks;
		action   = 1;
		buy      = 1;
		coin     = 1;
	    }
	  else
	    let _ =
	      Logger.debug "shuffle" () in
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
	let _ = send_all state @@ `Phase (`Action, name) in
	(_,state) <-- user p state skip;
	let _ = send_all state @@ `Phase (`Buy, name) in
	state <-- buy p client state;
	let _ = send_all state @@ `Phase (`Cleanup, name) in
	let game = cleanup 5 state.game in
	return @@ `End { state with game }
      end

  let invoke_turn state =
    let open Cc in
    let client =
      current_client state in
      save_cc client @@ perform begin
	p <-- new_prompt ();
	pushP p @@ turn p client state
      end

  let handle_player client request state : t =
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

  let handle client (req : Protocol.game_req) state =
    let state' =
      match req with
	  #common_request as r ->
	    handle_common client r state
	| `Ready when not state.playing ->
	    handle_ready client state
	| `Ready ->
	    S.send client @@ `Error "already started";
	    state
	| #player_request as r ->
	    if client = current_client state then
	      handle_player client r state
	    else begin
	      S.send client @@ `Error "not your turn";
	      state
	    end
	| `Create ->
	    failwith "must not happen" in
      if state'.playing && Hashtbl.length table = 0 then
	invoke_turn state'
      else
	state'
end
