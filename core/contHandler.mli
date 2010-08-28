open Base
open Cc

module type S = sig
  type client
  type request
  type state
end

module Make : functor(S : S) -> sig
  type cc = [
  | `Cc  of S.state * action
  | `End of S.state
  ]
  and action = (S.request -> bool) * (S.request -> S.state -> (unit, cc) Cc.CONT.mc)

  val run : (S.state -> (unit, cc) Cc.CONT.mc) -> S.client -> S.state -> unit
  val handle : S.client -> S.request -> S.state -> (S.state, string) Base.either
end
