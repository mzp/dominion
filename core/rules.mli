module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make : functor(S : S) -> sig
  type state
  val make : string -> state * 'a Game.t
  val run  : S.t -> Protocol.game_req -> (state * 'a Game.t) -> (state * 'a Game.t)
end
