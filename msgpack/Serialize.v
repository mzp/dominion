(* -*- coding: utf-8 -*- *)
Require Import List Ascii.
Require Import BigEndian Object.

Open Scope list_scope.
Open Scope char_scope.

Definition singleton {A} (x : A) := x :: nil.

(** MsgPackのシリアライズルールの定義 *)
Inductive Serialized : object -> list ascii8 -> Prop :=
| SNil  :
  Serialized Nil (singleton "192")
| STrue :
  Serialized (Bool true) (singleton "195")
| SFalse :
  Serialized (Bool false) (singleton "194")
| SPFixnum : forall x1 x2 x3 x4 x5 x6 x7,
  Serialized (PFixnum   (Ascii x1 x2 x3 x4 x5 x6 x7 false))
             (singleton (Ascii x1 x2 x3 x4 x5 x6 x7 false))
| SNFixnum : forall  x1 x2 x3 x4 x5,
  Serialized (NFixnum   (Ascii x1 x2 x3 x4 x5 true true true))
             (singleton (Ascii x1 x2 x3 x4 x5 true true true))
| SUint8 : forall c,
  Serialized (Uint8 c) ("204"::list_of_ascii8 c)
| SUint16 : forall c,
  Serialized (Uint16 c) ("205"::list_of_ascii16 c)
| SUint32 : forall c,
  Serialized (Uint32 c) ("206"::list_of_ascii32 c)
| SUint64 : forall c,
  Serialized (Uint64 c) ("207"::list_of_ascii64 c)
| SInt8 : forall c,
  Serialized (Int8 c) ("208"::list_of_ascii8 c)
| SInt16 : forall c,
  Serialized (Int16 c) ("209"::list_of_ascii16 c)
| SInt32 : forall c,
  Serialized (Int32 c) ("210"::list_of_ascii32 c)
| SInt64 : forall c,
  Serialized (Int64 c) ("211"::list_of_ascii64 c)
| SFloat : forall c,
  Serialized (Float c) ("202"::list_of_ascii32 c)
| SDouble : forall c,
  Serialized (Double c) ("203"::list_of_ascii64 c)
| SFixRaw : forall cs b1 b2 b3 b4 b5 b6 b7 b8,
  Ascii b1 b2 b3 b4 b5 b6 b7 b8 = ascii8_of_nat (length cs) ->
  Serialized (FixRaw cs) ((Ascii b1 b2 b3 b4 b5 true false true)::cs)
| SRaw16 : forall cs s1 s2,
  (s1,s2) =  ascii16_of_nat (length cs) ->
  Serialized (Raw16 cs) ("218"::s1::s2::cs)
| SRaw32 : forall cs s1 s2 s3 s4,
  ((s1,s2),(s3,s4)) =  ascii32_of_nat (length cs) ->
  Serialized (Raw32 cs) ("219"::s1::s2::s3::s4::cs).
