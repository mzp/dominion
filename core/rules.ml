open Base
module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make(S : S) = struct
  open S

  type request = {
    id : t;
  }

  type state = {
    name : string;
    name_table : (t * string) list
  }

  let make name = {
    name = name;
    name_table = [];
  }

  let rec lookup x = function
      [] -> None
    | (k,v)::ys ->
	if equal x k then
	  Some v
	else
	  lookup x ys

  let run id req s =
    match req with
    | `Join player ->
	  ret { s with
		  name_table    = (id, player) :: s.name_table } @@
	    send id `Ok
    | `Say msg ->
	ret s @@ ignore @@
	  Maybe.(perform begin
		   player <-- lookup id s.name_table;
		   let _ =
		     List.map fst s.name_table
		     +> List.iter (flip send @@ `Chat (player, msg)) in
		   return ()
		 end)
    | _ ->
	failwith "not yet"
end
