open Base
open Cc

(*
  'a : client
  'b : request
  'c : state
*)
type ('a,'b,'c) t
type ('a,'b,'c) cc

(** 処理を中断してrequestを待つ *)
type ('a,'b,'c) suspend = ('b -> bool) -> 'c -> (unit, 'b * 'c) Cc.CONT.mc

(** 継続サーバの生成 *)
val make : unit -> ('a,'b,'c) t

(** 処理を完了する *)
val end_ : 'c -> (unit, ('a, 'b, 'c) cc) Cc.CONT.mc

(** 処理を開始する *)
val start : ('a,'b,'c) t -> 'a -> 'c -> f:(('a,'b,'c) suspend -> 'c -> (unit, ('a,'b,'c) cc) Cc.CONT.mc) -> ('c,string) Base.either

(** 処理を再開する *)
val resume : ('a,'b,'c) t -> 'a -> 'b -> 'c -> ('c, string) Base.either
