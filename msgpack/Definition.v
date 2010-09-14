Require Import Ascii.
Require Import List.
Require Import Prefix.

Open Local Scope char_scope.
Open Local Scope list_scope.

Inductive data :=
| Bool (_ : bool)
| Nil
| PFixnum: forall n,
  nat_of_ascii n < 128 -> data
| NFixnum: forall n,
  (* 負の数を導入したくないので、補数表現を使う。 *)
  224 <= nat_of_ascii n /\ nat_of_ascii n <= 255 ->
  data
| Uint8  (_ : ascii)
| Uint16 (_ _ : ascii)
| Uint32 (_ _ _ _ : ascii)
| Uint64 (_ _ _ _ _ _ _ _ : ascii)
| Int8   (_ : ascii)
| Int16  (_ _ : ascii)
| Int32  (_ _ _ _ : ascii)
| Int64  (_ _ _ _ _ _ _ _ : ascii)
| Float  (_ _ _ _ : ascii)
| Double (_ _ _ _ _ _ _ _ : ascii).

Inductive eq_data : data -> data -> Prop :=
| BoolEq : forall b,
  eq_data (Bool b) (Bool b)
| NilEq  :
  eq_data Nil Nil
| PFixnumEq: forall n m P Q,
  n = m ->
  eq_data (PFixnum n P) (PFixnum m Q)
| NFixnumEq: forall n m P Q,
  n = m ->
  eq_data (NFixnum n P) (NFixnum m Q)
| Uint8Eq : forall c,
  eq_data (Uint8 c) (Uint8 c)
| Uint16Eq : forall c1 c2,
  eq_data (Uint16 c1 c2) (Uint16 c1 c2)
| Uint32Eq : forall c1 c2 c3 c4,
  eq_data (Uint32 c1 c2 c3 c4) (Uint32 c1 c2 c3 c4)
| Uint64Eq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_data (Uint64 c1 c2 c3 c4 c5 c6 c7 c8) (Uint64 c1 c2 c3 c4 c5 c6 c7 c8)
| Int8Eq : forall c,
  eq_data (Int8 c) (Int8 c)
| Int16Eq : forall c1 c2,
  eq_data (Int16 c1 c2) (Int16 c1 c2)
| Int32Eq : forall c1 c2 c3 c4,
  eq_data (Int32 c1 c2 c3 c4) (Int32 c1 c2 c3 c4)
| Int64Eq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_data (Int64 c1 c2 c3 c4 c5 c6 c7 c8) (Int64 c1 c2 c3 c4 c5 c6 c7 c8)
| FloatEq : forall c1 c2 c3 c4,
  eq_data (Float c1 c2 c3 c4) (Float c1 c2 c3 c4)
| DoubleEq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_data (Double c1 c2 c3 c4 c5 c6 c7 c8) (Double c1 c2 c3 c4 c5 c6 c7 c8).

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



Lemma PFixnum_inv : forall x y P Q,
  ~ eq_data (PFixnum x P) (PFixnum y Q) ->
  x <> y.
Proof.
intros.
intro.
apply H.
apply PFixnumEq.
tauto.
Qed.

Ltac try_all :=
  try (apply BoolEq);
  try (apply NilEq);
  try (apply PFixnumEq);
  try (apply NFixnumEq);
  try (apply Uint8Eq);
  try (apply Uint16Eq);
  try (apply Uint32Eq);
  try (apply Uint64Eq);
  try (apply Int8Eq);
  try (apply Int16Eq);
  try (apply Int32Eq);
  try (apply Int64Eq);
  try (apply FloatEq);
  try (apply DoubleEq); tauto.


Theorem NotPrefix : forall x1 x2 y1 y2,
  ~(eq_data x1 x2) ->
  Serialized x1 y1 ->
  Serialized x2 y2 ->
  ~ Prefix y1 y2.
Proof.
intros.
inversion H0; inversion H1;
  try( intro; apply H; rewrite <- H2, <- H4; apply BoolEq);
  try( intro; apply H; rewrite <- H2, <- H4; apply NilEq);
  try( apply not_prefix_singleton;
       intro;
       discriminate);
  try( unfold singleton;
       apply not_prefix_hd;
       intro;
       discriminate);
  try( apply not_prefix_singleton;
       intro;
       apply H;
       rewrite <- H2, <- H4;
       try_all;
       tauto);
  try( apply not_prefix_tl;
       apply not_prefix_hd;
       intro;
       apply H;
       rewrite <- H2,<- H4;
       rewrite H6;
       try_all).

  apply not_prefix_tl.
  destruct (ascii_dec x0 x4); try (apply not_prefix_hd; tauto).
  rewrite e in *.
  apply not_prefix_tl.
  apply not_prefix_hd.
  intro.
  apply H.
  rewrite <- H2,<- H4,<- H6.
  try_all.
Abort.

