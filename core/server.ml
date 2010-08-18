open Base
open Ccell
module type Transport = sig
  val connect : string -> int -> 'a Event.channel
  val server  : string -> int -> f:('a Event.channel -> unit) -> unit
end

module Make(T : Transport) = struct
  let run host port =
    let bc =
      Bcast.make () in
      T.server host port ~f:begin fun ch ->
	let port =
	  Bcast.make_port bc in
	while true do
	  ignore @@ Event.select [
	    Event.wrap (Event.receive ch) (fun s -> Bcast.send bc s);
	    Event.wrap (Bcast.receive_port port) (fun s -> Event.sync @@ Event.send ch s);
	  ]
	done
      end

  let connect host port =
    let ch =
      T.connect host port in
    let _ =
      Thread.create
	(fun () ->
	   while true do
	     Printf.printf "--> %s\n" @@ Event.sync @@ Event.receive ch
	   done)
	() in
      while true do
	Event.sync @@ Event.send ch @@ read_line ()
      done
end
