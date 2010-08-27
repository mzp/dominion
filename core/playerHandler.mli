module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state
  val invoke : state -> state
  val handle : S.t -> request -> state -> state

  val action : ([> `Cc of
		   state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> state -> (unit, state) Cc.CONT.mc

  val buy : ([> `Cc of
		   state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> state -> (unit, state) Cc.CONT.mc

  val cleanup : ([> `Cc of
		   state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> state -> (unit, state) Cc.CONT.mc

  val game : state -> Game.t
  val make_dummy : S.t list -> Game.t -> state
end
