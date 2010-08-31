open Base
open Cc

module type S = sig
  type client
  type request
  type state
end

module Make : functor(S : S) -> sig
  type t

  (* 処理を中断してrequestを待つ *)
  type suspend = (S.request -> bool) -> S.state -> (unit, S.request * S.state) Cc.CONT.mc
  (* 処理を完了する *)
  val end_ : S.state -> (unit, t) Cc.CONT.mc

  (* 処理を開始する *)
  val start : (suspend -> S.state -> (unit, t) Cc.CONT.mc) -> S.client -> S.state -> (S.state,string) Base.either

  (* 処理を再開する *)
  val resume : S.client -> S.request -> S.state -> (S.state, string) Base.either
end

