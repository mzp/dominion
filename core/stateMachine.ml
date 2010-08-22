open Base

module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make(S : S) = struct
  type phase = [
    `GameInit
  | `Action
  | `Buy
  | `Cleanup
  ]

  (* utility function *)
  let rec lookup x = function
      [] -> None
    | (k,v)::ys ->
	if S.equal x k then
	  Some v
	else
	  lookup x ys

  let rec mem key xs =
    lookup key xs <> None

  let add (key, value) xs =
    if mem key xs then
      xs
    else
      (key,value) :: xs

  let shuffle xs =
    Random.self_init ();
    List.map (fun x -> (Random.int (List.length xs), x)) xs
    +> List.sort (fun (x,_) (y,_) -> compare x y)
    +> List.map snd

  (* state transition *)
  type state = {
    clients : (S.t * string) list;
    game    : Game.t;
    ready   : int
  }

  let init_state = {
    clients = [];
    game    = Game.make [] [];
    ready   = 0
  }

  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)

  let find p xs =
    (option (List.find p)) xs

  let player_of_client client s =
    Maybe.(perform begin
	     name <-- lookup client s.clients;
	     Game.(find (fun p -> p.name = name) s.game.players)
	   end)

  let no_trans x =
    (x,None)

  module type State = sig
    val init    : state -> state * phase option
    val request : S.t -> Protocol.game_req -> state -> state * phase option
  end

  module Common = struct
    let init s =
      (s, None)

    let request client req s =
      match req with
	| `Join name ->
	    S.send client `Ok;
	    no_trans { s with
			 clients = (client, name) :: s.clients; }
	| `Say msg ->
	    ignore @@ Maybe.(perform begin
			       name <-- lookup client s.clients;
			       return @@ send_all s @@ `Chat (name, msg)
			     end);
	    no_trans s
	| `Query `Supply ->
	    let xs =
	      Game.(List.map to_string s.game.board.supply) in
	    let _ =
	      S.send client @@ `Cards xs in
	      no_trans s
	| `Query `Mine ->
	    let open Game in
	      (S.send client @@
		 match player_of_client client s with
		     Some { hands; _ } ->
		       `Cards (List.map to_string hands)
		   | None ->
		       `Error "not join");
	      no_trans s
	| _ ->
	    S.send client (`Error "invalid request");
	    no_trans s
  end

  module GameInit : State = struct
    let init = Common.init

    let game_start s =
      let players =
	ListLabels.map s.clients ~f:begin fun (_,name)->
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
      let game =
	Game.make (shuffle players) cards in
      let _ =
	send_all s `GameStart in
	({ s with game = game }, Some `Action)

    let request client req s =
      match req with
	| `Join name ->
	    let ({ ready; _ } as s', _) =
	      Common.request client req s in
	      no_trans { s' with ready = 1 + ready }
	| `Ready ->
	    if mem client s.clients then
	      let s' =
		{ s with ready = s.ready - 1 } in
		if s'.ready = 0 then
		  game_start s'
		else
		  no_trans s'
	    else
	      (S.send client @@ `Error "not join";
	       no_trans s)
	| _ ->
	    Common.request client req s
  end

  module Action = struct
    let init s =
      let open Game in
	send_all s @@ `Turn (List.nth s.game.players s.game.me).name;
	(s,None)

    let request = Common.request
  end

  module Buy = struct
    let init = Common.init
    let request = Common.request
  end

  module Cleanup = struct
    let init = Common.init
    let request = Common.request
  end

  let to_module = function
      `GameInit ->
	(module GameInit : State)
    | `Action ->
	(module Action : State)
    | `Buy ->
	(module Buy : State)
    | `Cleanup ->
	(module Cleanup : State)


  type t = state * phase

  let initial : t =
    init_state, `GameInit

  let rec transition (state, phase) =
    let module M =
      (val to_module phase : State) in
    let (state', phase') =
      M.init state in
      match phase' with
	  Some next_phase ->
	    transition (state', next_phase)
	| None ->
	    state', phase

  let request client req (state, phase) =
    let module M =
      (val to_module phase : State) in
    let (state', phase') =
	M.request client req state in
      match phase' with
	| Some x ->
	    transition (state', x)
	| None ->
	    state', phase
end
