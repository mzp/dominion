module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Ready
  ]
  val handle : S.t -> request -> B.state -> B.state
end
