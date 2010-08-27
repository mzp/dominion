module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state
  val invoke : state -> state
  val handle : S.t -> request -> state -> state

  type client = {
    client : S.t;
    me     : Game.player;
    prompt : ([ `Cc of state * ([ `Select of Game.card | `Skip ] -> bool) *
		  ([ `Select of Game.card | `Skip ] -> state -> (unit, 'a2) Cc.CONT.mc)
	      | `End of state ] as 'a2)  Cc.prompt
  }

  val action : client -> state -> (unit, state) Cc.CONT.mc
  val buy : client -> state -> (unit, state) Cc.CONT.mc
  val cleanup : client -> state -> (unit, state) Cc.CONT.mc

  val game : state -> Game.t
  val make_dummy : S.t list -> Game.t -> state
end
