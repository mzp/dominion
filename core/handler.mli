module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make : functor(S : S) -> sig
  type t

  val initial : t
  val handle : S.t -> Protocol.game_req -> t -> t

  (* for test *)
  (* fixme: type signature is uglllllyyyy *)
  val action : ([> `Cc of
        t * ([> `Select of 'b2 | `Skip ] -> bool) *
          ([< `Select of Game.card | `Skip ] -> t -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> t -> (unit, t) Cc.CONT.mc
  val buy : ([> `Cc of
        t * ([> `Select of 'b2 | `Skip ] -> bool) *
        ([< `Select of Game.card | `Skip ] -> t -> (unit, 'a2) Cc.CONT.mc) ] as 'a2)  Cc.prompt -> S.t -> t -> (unit, t) Cc.CONT.mc
  val cleanup : int -> Game.t -> Game.t
end
