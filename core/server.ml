open Base
module type Transport = sig
  type 'a channel
  type 'a event
  val send : 'a channel -> 'a -> unit event
  val receive : 'a channel -> 'a event
  val choose : 'a event list -> 'a event
  val sync : 'a event -> 'a

  val connect : host:string -> port:int -> 'a channel
  val server  : host:string -> port:int -> f:('a channel -> unit) -> unit
end

module Make(T : Transport) = struct
  let run host port =
    T.server ~host ~port ~f:begin fun (ch : string T.channel) ->
      T.sync @@ T.send ch "hi"
    end

  let connect host port =
    let ch =
      T.connect host port in
    let s =
      T.sync @@ T.receive ch in
      print_endline s
end


