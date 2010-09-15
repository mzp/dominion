Require Import List.
Require Import Ascii.
Require Import Data.

Open Scope list_scope.
Open Scope char_scope.

Definition singleton {A} (x : A) := x :: nil.

(* MsgPackのシリアライズルールの定義 *)
Inductive Serialized : data -> list ascii -> Prop :=
| SNil  :
  Serialized Nil (singleton "192")
| STrue :
  Serialized (Bool true) (singleton "195")
| SFalse :
  Serialized (Bool false) (singleton "194")
| SPFixnum : forall x1 x2 x3 x4 x5 x6 x7 P,
  Serialized (PFixnum   (Ascii x1 x2 x3 x4 x5 x6 x7 false) P)
             (singleton (Ascii x1 x2 x3 x4 x5 x6 x7 false))
| SNFixnum : forall  x1 x2 x3 x4 x5 P,
  Serialized (NFixnum   (Ascii x1 x2 x3 x4 x5 true true true) P)
             (singleton (Ascii x1 x2 x3 x4 x5 true true true))
| SUint8 : forall x1,
  Serialized (Uint8 x1) ("204"::x1::nil)
| SUint16 : forall x1 x2,
  Serialized (Uint16 x1 x2) ("205"::x1::x2::nil)
| SUint32 : forall x1 x2 x3 x4,
  Serialized (Uint32 x1 x2 x3 x4) ("206"::x1::x2::x3::x4::nil)
| SUint64 : forall x1 x2 x3 x4 x5 x6 x7 x8,
  Serialized (Uint64 x1 x2 x3 x4 x5 x6 x7 x8) ("207"::x1::x2::x3::x4::x5::x6::x7::x8::nil)
| SInt8 : forall x1,
  Serialized (Int8 x1) ("208"::x1::nil)
| SInt16 : forall x1 x2,
  Serialized (Int16 x1 x2) ("209"::x1::x2::nil)
| SInt32 : forall x1 x2 x3 x4,
  Serialized (Int32 x1 x2 x3 x4) ("210"::x1::x2::x3::x4::nil)
| SInt64 : forall x1 x2 x3 x4 x5 x6 x7 x8,
  Serialized (Int64 x1 x2 x3 x4 x5 x6 x7 x8) ("211"::x1::x2::x3::x4::x5::x6::x7::x8::nil)
| SFloat : forall x1 x2 x3 x4,
  Serialized (Float x1 x2 x3 x4) ("202"::x1::x2::x3::x4::nil)
| SDouble : forall x1 x2 x3 x4 x5 x6 x7 x8,
  Serialized (Double x1 x2 x3 x4 x5 x6 x7 x8) ("203"::x1::x2::x3::x4::x5::x6::x7::x8::nil).
