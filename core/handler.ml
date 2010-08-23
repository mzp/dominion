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

  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)

  let rec lookup x = function
      [] -> None
    | (k,v)::ys ->
	if S.equal x k then
	  Some v
	else
	  lookup x ys

  let rec mem key xs =
    lookup key xs <> None

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
	Game.make (shuffle players) cards

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
	    game = game state' }
      end else
	state'

  type player_request = [
  | `Select of Game.card
  | `Skip
  ]

  let table : (S.t, ((player_request -> bool) *
                       (S.t -> player_request -> t ->
			  (unit, [ `Cc of t * (player_request -> bool) * 'b | `End of t ])
			    Cc.CONT.mc as 'b))) Hashtbl.t =
    Hashtbl.create 0

  let save_cc client : 'a -> t = function
      `End state ->
	Hashtbl.clear table;
	state
    | `Cc (state, pred, cc) ->
	Hashtbl.add table client (pred, cc);
	state

  let skip prompt state =
    let open Cc in
    let p =  function
	`Skip ->
	  true
      | _ ->
	  false in
    let handle k _ _ _ =
      k @@ return () in
      shiftP prompt (fun k -> return @@ `Cc(state, p, handle k))

  let turn p client _request state =
    let open Cc in
      perform begin
	let _ = S.send client @@ `Error "input skip" in
	() <-- skip p state;
	let _ = S.send client @@ `Error "yahoo" in
	return @@ `End state
      end

  let handle_player client request state : t =
    let open Cc in
      if Hashtbl.mem table client then
	let (p, k) =
	  Hashtbl.find table client in
	  if p request then
	    save_cc client @@ Cc.run @@ k client request state
	  else begin
	    S.send client @@ `Error "invalid request";
	    state
	  end
      else
	save_cc client @@ Cc.run @@ perform begin
	  p <-- new_prompt ();
	  pushP p @@ turn p client request state
	end

  let handle client (req : Protocol.game_req) state =
    match req with
	#common_request as r ->
	  handle_common client r state
      | `Ready when not state.playing ->
	  handle_ready client state
      | `Ready ->
	  S.send client @@ `Error "already started";
	  state
      | #player_request as r ->
	  handle_player client r state
      | `Create ->
	  failwith "must not happen"
end
