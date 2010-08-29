module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Join of string
  | `Query of [`Supply | `Mine ]
  | `Say of string
  ]
  type state = S.t HandlerBase.state
  val handle : S.t -> request -> state -> (state,string) Base.either
end
