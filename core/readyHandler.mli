module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Ready
  ]
  type state = S.t HandlerBase.state
  val handle : S.t -> request -> state -> (state, string) Base.either
end
