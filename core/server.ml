open Base
module type Transport = sig
  val connect : string -> int -> 'a Event.channel
  val server  : string -> int -> f:('a Event.channel -> unit) -> unit
end

module Make(T : Transport) = struct
  let run host port =
    T.server host port ~f:begin fun ch ->
      Event.sync @@ Event.send ch "hi"
    end

  let connect host port =
    let ch =
      T.connect host port in
      print_endline @@ Event.sync @@ Event.receive ch
(*    let _ =
      Thread.create
	(fun () ->
	   Printf.printf "--> %s\n" @@ T.sync @@ T.receive ch)
	() in
      while true do
	T.sync @@ T.send ch @@ read_line ()
      done*)
end
