open Base
open Ccell
open Protocol
open Printf

module Make(T : Protocol.S) = struct
  let strip s =
    try
      let n =
	String.index s '\000' in
	String.sub s 0 n
    with Not_found ->
      s

  let game =
    ref ""

  let send ch e =
    Event.sync @@ Event.send ch e

  let make_id () =
    string_of_int @@ Random.int 100

  let parse id line =
    match Str.split (Str.regexp " ") @@ line with
	["/ls"] ->
	  `List id
      | ["/make"; name] ->
	  `Make (id, name)
      | ["/join"; x; y] ->
	  game := x;
	  `Game (x,(`Query (id, `Join y)))
      | "/say"::msgs ->
	  `Game (!game,
		 `Message (String.concat " " msgs))
      | ["/ready"] ->
	  `Game (!game,`Query (id,`Ready))
      | ["/query"; "supply"] ->
	  `Game (!game,
		 `Query (id, `List `Supply))
      | ["/query"; "mine"] ->
	  `Game (!game,
		 `Query (id, `List `Mine))
      | ["/skip"] ->
	  `Game (!game,`Query (id,`Skip))
      | ["/select"; c] ->
	  `Game (!game,`Query (id, `Select (Game.of_string c)))
      | ["/q"] ->
	  exit 0
      | _ ->
	  failwith "unexpect line"

  let get_id =  function
    | `Ok id | `Error (id,_) | `Games (id,_) | `Cards (id,_) ->
	Some id
    | `Message _ ->
	None

  let to_string = function
    | `Ok _ ->
	"ok"
    | `Error (_,s) ->
	sprintf "error: %s" s
    | `Games (_,xs) ->
	Std.dump xs
    | `Cards (_,xs) ->
	string_of_list Game.to_string xs
    | `Message (game, `Player(name,msg)) ->
	sprintf "%s@%s: %s" name game msg
    | `Message (game, `System(msg)) ->
	sprintf "%s: %s" game msg
    | `Message (game, `GameStart) ->
	sprintf "start@%s" game
    | `Message (game, `Turn name) ->
	sprintf "%s's turn@%s" name game
    | `Message (game, `ActionPhase name) ->
	sprintf "%s's action phase@%s" name game
    | `Message (game, `BuyPhase name) ->
	sprintf "%s's buy phase@%s" name game
    | `Message (game, `CleanupPhase name) ->
	sprintf "%s's cleanup phase@%s" name game

  let rec wait_for id response =
    Event.sync @@
      Event.wrap (Event.receive response)
      (fun v ->
	 print_endline @@ to_string v;
	 match get_id v with
	     Some id' when id = id' ->
	       ()
	   | Some _ | None ->
	       wait_for id response)

  let loop request response read =
    while true do
      let id =
	make_id () in
	try
	  Event.sync @@ Event.send request @@ parse id @@ read ();
	  wait_for id response
	with
	  | End_of_file ->
	      exit 0
	  | e ->
	      prerr_string (Printexc.to_string e);
	      prerr_string "\n"
    done

  let connect host port =
    let {res; req; _ } =
      T.connect host port in
      loop req res (fun () ->
		      read_line ())
end
