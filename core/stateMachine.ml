open Base
module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make(S : S) = struct
  type phase = [
    `GameInit
  | `TurnInit
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

  (* state transition *)
  type state = {
    clients : (S.t * string) list;
    game    : Game.t option;
  }

  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)

  module type State = sig
    val request : S.t -> Protocol.game_req -> state -> state * phase option
  end

  module Common = struct
    let request client req s =
      match req with
	| `Join name ->
	    S.send client `Ok;
	    { s with
		clients = (client, name) :: s.clients; },
	    None
	| `Say msg ->
	    ignore @@ Maybe.(perform begin
			       name <-- lookup client s.clients;
			       return @@ send_all s @@ `Chat (name, msg)
			     end);
	    s, None
	| _ ->
	    assert false
  end

  module GameInit = struct
    let request = Common.request
  end

  module TurnInit = struct
    let request = Common.request
  end

  module Action = struct
    let request = Common.request
  end

  module Buy = struct
    let request = Common.request
  end

  module Cleanup = struct
    let request = Common.request
  end

  let to_module = function
      `GameInit ->
	(module GameInit : State)
    | `TurnInit ->
	(module TurnInit : State)
    | `Action ->
	(module Action : State)
    | `Buy ->
	(module Buy : State)
    | `Cleanup ->
	(module Cleanup : State)


  type t = state * phase

  let initial : t =
    {
      clients = [];
      game    = None
    }, `GameInit

  let request client req (state, phase) =
    let module M =
      (val to_module phase : State) in
    let (state', phase') =
	M.request client req state in
      match phase' with
	  Some x ->
	    state', x
	| None ->
	    state', phase
end
