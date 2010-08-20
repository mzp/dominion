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
    phase : [`Init | `Action | `Buy ];
    name : string;
    count : int;
    name_table : (t * string) list;
  }

  let make name = {
    name = name;
    name_table = [];
    phase = `Init;
    count = 0;
  }, Game.empty

  let rec lookup x = function
      [] -> None
    | (k,v)::ys ->
	if equal x k then
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

  let send_all s x =
    List.map fst s.name_table
    +> List.iter (flip send x)

  let run id req (s, game) =
    match s.phase, req with
    | _, `Join player ->
	  ret ({ s with
		   count      = 1 + s.count;
		   name_table = add (id,player) s.name_table },
	       Game.({ game with
			 others = { empty_player with
				      player_name = player }
			 :: game.others } )) @@
	    send id `Ok
    | _, `Say msg ->
	ret (s, game) @@ ignore @@
	  Maybe.(perform begin
		   player <-- lookup id s.name_table;
		   let _ = send_all s @@ `Chat (player, msg) in
		   return ()
		 end)
    | `Init, `Ready ->
	if mem id s.name_table then
	  (send id `Ok;
	   if s.count - 1 = 0 then
	     ret ({s with
		     phase=`Action;
		     count = 0},
		  Game.({game with
			   me = List.hd game.others;
			   others = List.tl game.others})) @@
	       send_all s `GameStart
	   else
	     ({s with
		 count = s.count - 1},
	      game))
	else
	  ret (s,game) @@
	    send id (`Error "not join")
    | _ ->
	failwith "not yet"
end
