open Base
open Ccell
open ThreadUtils

module Make(T : Protocol.S) = struct
  let connect host port =
    let (w,r) =
      T.connect host port in
    let _ =
      daemon begin fun () ->
	match Event.sync @@ Event.receive r with
	  | `Rooms [] ->
	      p "no room" ()
	  | `Rooms xs ->
	      List.iter (fun x -> p "%s" x ()) xs
	  | `Chat msg ->
	      p "%s" msg ()
	  | `Ok ->
	      p "ok" ()
      end in
    let f _ = begin
      print_string "$ ";
      flush stdout;
      match Str.split (Str.regexp " ") @@ read_line () with
	  ["ls"] ->
	    Event.sync @@ Event.send w `ListRoom
	| ["make"; name] ->
	    Event.sync @@ Event.send w (`MakeRoom name)
	| ["connect"; name] ->
	    Event.sync @@ Event.send w (`Connect name)
	| "chat"::room::msgs ->
	    Event.sync @@ Event.send w (`Chat (room,String.concat " " msgs))
	| _ ->
	    ()
    end in
      forever f ()
end
