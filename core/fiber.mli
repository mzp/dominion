(** 途中でユーザからの入力を待つためのThreadみたいなライブラリ *)
open Base
open Cc

(** 'aが結果の型。
    'bが待ち受ける型。 *)
type ('a,'b) t
type ('a,'b) cc

(** 処理を中断してrequestを待つ *)
type ('a,'b) suspend = 'a -> (unit, 'b) Cc.CONT.mc

(** 処理を開始する *)
val create : (('a,'b) suspend -> (unit, ('a,'b) cc) Cc.CONT.mc) -> ('a,'b) t

(** 処理を再開する *)
val resume : ('a,'b) t -> 'b -> unit

(* 値を取得する *)
val value : ('a,'b) t -> 'a

(** 処理を完了する *)
val end_ : 'a -> (unit, ('a, 'b) cc) Cc.CONT.mc

val is_alive : ('a,'b) t -> bool
