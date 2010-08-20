module Make : functor (T : Protocol.S) -> sig
  val connect : string -> int -> unit
end
