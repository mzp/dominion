module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Join of string
  | `Query of [`Supply | `Mine ]
  | `Say of string
  ]
  val handle : S.t -> request -> B.state -> (B.state,string) Base.either
end
