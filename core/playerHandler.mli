module Make : functor(S : Protocol.Rpc) -> functor(B : HandlerBase.S with type t = S.t) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  val invoke : B.state -> B.state
  val handle : S.t -> request -> B.state -> B.state

  val action : ([> `Cc of
		   B.state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> B.state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> B.state -> (unit, B.state) Cc.CONT.mc

  val buy : ([> `Cc of
		   B.state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> B.state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> B.state -> (unit, B.state) Cc.CONT.mc

  val cleanup : ([> `Cc of
		   B.state * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> B.state -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> B.state -> (unit, B.state) Cc.CONT.mc

  val game : B.state -> Game.t
  val make_dummy : S.t list -> Game.t -> B.state
end
