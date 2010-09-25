(* -*- coding: utf-8 -*- *)
Require Import List Ascii.
Require Import BigEndian Object.

Open Scope list_scope.
Open Scope char_scope.

Definition singleton {A} (x : A) := x :: nil.

Definition Map {A B} P (xs : list A) (ys : list B) := forall i x y,
  value x = nth_error xs i ->
  value y = nth_error ys i ->
  P x y.

Definition lift {A B} (P : A -> B -> Prop) (p1 : (A*A)%type) ( p2 : (B*B)%type) :=
  let (x1, x2) := p1 in
  let (y1, y2) := p2 in
    P x1 y1 /\ P x2 y2.


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
  Serialized (Raw32 cs) ("219"::s1::s2::s3::s4::cs)
| SFixArray : forall xs ys b1 b2 b3 b4 b5 b6 b7 b8,
  Ascii b1 b2 b3 b4 b5 b6 b7 b8 = ascii8_of_nat (length xs) ->
  Map Serialized xs ys ->
  Serialized (FixArray xs) ((Ascii b1 b2 b3 b4 true false false true)::flat_map (fun x=>x) ys)
| SArray16 : forall xs ys s1 s2,
  (s1,s2) = ascii16_of_nat (length xs) ->
  Map Serialized xs ys ->
  Serialized (Array16 xs) ("220"::s1::s2::flat_map (fun x=>x) ys)
| SArray32 : forall xs ys s1 s2 s3 s4,
  ((s1,s2),(s3,s4)) = ascii32_of_nat (length xs) ->
  Map Serialized xs ys ->
  Serialized (FixArray xs) ("221"::s1::s2::s3::s4::flat_map (fun x=>x) ys)
| SFixMap : forall xs ys b1 b2 b3 b4 b5 b6 b7 b8,
    Ascii b1 b2 b3 b4 b5 b6 b7 b8 = ascii8_of_nat (length xs) ->
    Map (fun x (y : (list ascii8 * list ascii8))=>
      Serialized (fst x) (fst y) /\ Serialized (snd x) (snd y)) xs ys ->
    Serialized (FixMap xs) ((Ascii b1 b2 b3 b4 false false false true)::flat_map (fun p => (fst p) ++ (snd p)) ys)
| SMap16 : forall xs ys s1 s2,
  (s1,s2) = ascii16_of_nat (length xs) ->
  Map (fun x (y : (list ascii8 * list ascii8))=>
    Serialized (fst x) (fst y) /\ Serialized (snd x) (snd y)) xs ys ->
  Serialized (Map16 xs) ("220"::s1::s2::flat_map (fun p => (fst p) ++ (snd p)) ys)
| SMap32 : forall xs ys s1 s2 s3 s4,
  ((s1,s2),(s3,s4)) = ascii32_of_nat (length xs) ->
  Map (fun x (y : (list ascii8 * list ascii8))=>
    Serialized (fst x) (fst y) /\ Serialized (snd x) (snd y)) xs ys ->
  Serialized (FixMap xs) ("221"::s1::s2::s3::s4::flat_map (fun p => (fst p) ++ (snd p)) ys).

