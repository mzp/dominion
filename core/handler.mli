module Make : functor(S : Protocol.Rpc) -> sig
  type t

  val initial : t
  val handle : S.t -> Protocol.game_req -> t -> t
end
