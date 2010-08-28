module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state
  val invoke : state -> state
  val handle : S.t -> request -> state -> state


  (* for test *)
  type cc = [
    `Cc of state * (request -> bool) * (request -> state -> (unit, cc) Cc.CONT.mc)
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
