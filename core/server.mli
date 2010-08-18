module type Transport =
  sig
    type 'a channel
    type 'a event
    val send : 'a channel -> 'a -> unit event
    val receive : 'a channel -> 'a event
    val choose : 'a event list -> 'a event
    val sync : 'a event -> 'a
    val connect : host:string -> port:int -> 'a channel
    val server : host:string -> port:int -> f:('a channel -> unit) -> unit
  end

module Make : functor (T : Transport) -> sig
  val run : string -> int  -> unit
  val connect : string -> int -> unit
end
