(* -*- coding: utf-8 -*- *)
Require Import List.
Require Import BigEndian.

(** MsgPackで使うオブジェクトの定義 *)
Inductive object :=
| Bool (_ : bool)
| Nil
| PFixnum (_ : ascii8)
| NFixnum (_ : ascii8)
| Uint8  (_ : ascii8)
| Uint16 (_ : ascii16)
| Uint32 (_ : ascii32)
| Uint64 (_ : ascii64)
| Int8   (_ : ascii8)
| Int16  (_ : ascii16)
| Int32  (_ : ascii32)
| Int64  (_ : ascii64)
| Float  (_ : ascii32)
| Double (_ : ascii64)
| FixRaw (_ : list ascii8)
| Raw16  (_ : list ascii8)
| Raw32  (_ : list ascii8).

(** 妥当なオブジェクトの定義 *)
Inductive Valid : object -> Prop :=
| VPFixNum : forall n,
  nat_of_ascii8 n < 128 -> Valid (PFixnum n)
| VNFixNum : forall n,
  (* 負の数を導入したくないので、補数表現を使う *)
  223 < nat_of_ascii8 n /\ nat_of_ascii8 n < 256 -> Valid (NFixnum n)
| VFixRaw : forall xs,
  length xs < pow 5 -> Valid (FixRaw xs)
| VRaw16 : forall xs,
  length xs < pow 16 -> Valid (Raw16 xs)
| VRaw32 : forall xs,
  length xs < pow 32 -> Valid (Raw32 xs).
