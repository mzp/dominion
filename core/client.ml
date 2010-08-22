open Base
open Ccell
open ThreadUtils
open Protocol

module Make(T : Protocol.S) = struct
  let connect host port =
    let {req; res} =
      T.connect host port in
    let _ =
      daemon begin fun () ->
	match Event.sync @@ Event.receive res with
	  | `Games xs ->
	      List.iter (fun x -> p "%s" x ()) xs
	  | `Chat (name,msg) ->
	      p "[%s]%s" name msg ()
	  | `Ok ->
	      p "ok" ()
	  | `Error s ->
	      p "error: %s" s ()
	  | `GameStart ->
	      p "game start" ()
	  | `Cards xs ->
	      List.iter (fun x -> p "%s" x ()) xs
      end in
    let game =
	ref "" in
    let f _ = begin
      print_string "$ ";
      flush stdout;
      match Str.split (Str.regexp " ") @@ read_line () with
	  ["/rooms"] ->
	    Event.sync @@ Event.send req `List
	| ["/room"; name] ->
	    Event.sync @@ Event.send req (`Game (name,`Create))
	| ["/join"; x; y] ->
	    game := x;
	    Event.sync @@ Event.send req (`Game (x,`Join y))
	| "/chat"::msgs ->
	    Event.sync @@ Event.send req (`Game (!game,
						 `Say (String.concat " " msgs)))
	| ["/ready"] ->
	    Event.sync @@ Event.send req (`Game (!game,`Ready))
	| ["/query"; "supply"] ->
	    Event.sync @@ Event.send req (`Game (!game,`Query `Supply))
	| ["/query"; "mine"] ->
	    Event.sync @@ Event.send req (`Game (!game,`Query `Mine))
	| _ ->
	    ()
    end in
      forever f ()
end
