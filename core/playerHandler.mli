module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state = S.t HandlerBase.state
  val invoke : state -> unit
  val handle : S.t -> request -> state -> (state,string) Base.either

  (* for test *)
  type cc = [
    `Cc of state * ((request -> bool) * (request -> state -> (unit, cc) Cc.CONT.mc))
  | `End of state
  ]

  type client = {
    client : S.t;
    prompt : cc Cc.prompt
  }

  val action : client -> state -> (unit, state) Cc.CONT.mc
  val buy : client -> state -> (unit, state) Cc.CONT.mc
  val cleanup : client -> state -> (unit, state) Cc.CONT.mc

  val game : state -> Game.t
  val make_dummy : S.t list -> Game.t -> state
end
