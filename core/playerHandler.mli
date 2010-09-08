val observer : Game.t Observer.t

module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state = S.t HandlerBase.state
  val invoke : state -> unit
  val handle : S.t -> request -> state -> (state,string) Base.either
end
