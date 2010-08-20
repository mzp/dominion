module Make : functor (T : Protocol.S) -> sig
  val run : string -> int  -> unit
end
