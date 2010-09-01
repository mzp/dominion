module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state = S.t HandlerBase.state
  type client = {
    client : S.t;
    suspend: (S.t, request, state) ContHandler.suspend
  }

  val invoke : state -> unit
  val handle : S.t -> request -> state -> (state,string) Base.either

  (* for test *)
  val card_action   : Game.card -> client -> state -> (unit, state) Cc.CONT.mc
  val action_phase  : client -> state -> (unit, state) Cc.CONT.mc
  val buy_phase     : client -> state -> (unit, state) Cc.CONT.mc
  val cleanup_phase : client -> state -> (unit, state) Cc.CONT.mc

  val game : state -> Game.t
  val make_dummy : S.t list -> Game.t -> state
end
