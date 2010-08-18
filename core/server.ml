open Base
open Ccell
module type Transport = sig
  val connect : string -> int -> ('a Event.channel*'a Event.channel)
  val server  : string -> int -> f:('a Event.channel -> 'a Event.channel -> unit) -> unit
end

module Make(T : Transport) = struct
  let run host port =
    let bc =
      Bcast.make () in
      T.server host port ~f:begin fun r w ->
	while true do
	  ignore @@ Event.select [
	    Event.wrap (Event.receive r)
	      (fun s ->
		 Bcast.send bc s);
	    Event.wrap (Bcast.receive bc)
	      (fun s ->
		 Event.sync @@ Event.send w s);
	  ]
	done
      end

  let connect host port =
    let (r,w) =
      T.connect host port in
    let _ =
      Thread.create
	(fun () ->
	   while true do
	     Printf.printf "--> %s\n" @@ Event.sync @@ Event.receive r;
	     flush stdout;
	   done)
	() in
      while true do
	Event.sync @@ Event.send w @@ read_line ();
	Unix.sleep 1
      done
end
