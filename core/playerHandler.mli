module Make : functor(S : Protocol.Rpc) -> sig
  type request = [
  | `Select of Game.card
  | `Skip
  ]

  type state = S.t HandlerBase.state
  val invoke : state -> unit
  val handle : S.t -> request -> state -> (state,string) Base.either

  (* for test *)
  type t = {
    me     : string;
    others  : string list;
    request : string -> request Rule.t
  }
  val card_action   : t -> Game.card -> unit Rule.t
  val action_phase  : t -> unit Rule.t
  val buy_phase     : t -> unit Rule.t
  val cleanup_phase : t -> unit Rule.t

  val game : state -> Game.t
  val make_dummy : S.t list -> Game.t -> state
end
