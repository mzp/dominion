Require Import List.
Require Import Ascii.
Require Import BigEndian.
Require Import Object.

Open Scope list_scope.
Open Scope char_scope.

Definition singleton {A} (x : A) := x :: nil.

(* MsgPackのシリアライズルールの定義 *)
Inductive Serialized : object -> list ascii8 -> Prop :=
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
| SUint8 : forall c,
  Serialized (Uint8 c) ("204"::ascii8_of_8 c)
| SUint16 : forall c,
  Serialized (Uint16 c) ("205"::ascii8_of_16 c)
| SUint32 : forall c,
  Serialized (Uint32 c) ("206"::ascii8_of_32 c)
| SUint64 : forall c,
  Serialized (Uint64 c) ("207"::ascii8_of_64 c)
| SInt8 : forall c,
  Serialized (Int8 c) ("208"::ascii8_of_8 c)
| SInt16 : forall c,
  Serialized (Int16 c) ("209"::ascii8_of_16 c)
| SInt32 : forall c,
  Serialized (Int32 c) ("210"::ascii8_of_32 c)
| SInt64 : forall c,
  Serialized (Int64 c) ("211"::ascii8_of_64 c)
| SFloat : forall c,
  Serialized (Float c) ("202"::ascii8_of_32 c)
| SDouble : forall c,
  Serialized (Double c) ("203"::ascii8_of_64 c).
