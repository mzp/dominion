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
	  | _ ->
	      p "unknown response" ()
      end in
    let game =
	ref "" in
    let f _ = begin
      print_string "$ ";
      flush stdout;
      match Str.split (Str.regexp " ") @@ read_line () with
	  ["ls"] ->
	    Event.sync @@ Event.send req `List
	| ["make"; name] ->
	    Event.sync @@ Event.send req (`Game (name,`Create))
	| ["connect"; x; y] ->
	    game := x;
	    Event.sync @@ Event.send req (`Game (x,`Join y))
	| "chat"::msgs ->
	    Event.sync @@ Event.send req (`Game (!game,
					       `Say (String.concat " " msgs)))
	| _ ->
	    ()
    end in
      forever f ()
end
