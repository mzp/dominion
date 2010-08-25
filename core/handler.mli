module type S = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> Protocol.response -> unit
end

module Make : functor(S : S) -> sig
  type t
  val initial : t
  val handle : S.t -> Protocol.game_req -> t -> t
end
