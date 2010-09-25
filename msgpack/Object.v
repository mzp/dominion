Require Import List.
Require Import BigEndian.

Inductive object :=
| Bool (_ : bool)
| Nil
| PFixnum: forall n,
  nat_of_ascii8 n < 128 -> object
| NFixnum: forall n,
  (* 負の数を導入したくないので、補数表現を使う。 *)
  223 < nat_of_ascii8 n /\ nat_of_ascii8 n < 256 ->
  object
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

(* データの等値性を定義。依存型の証明部分は無視する。*)
Inductive eq_object : object -> object -> Prop :=
| BoolEq : forall b,
  eq_object (Bool b) (Bool b)
| NilEq  :
  eq_object Nil Nil
| PFixnumEq: forall n m P Q,
  n = m ->
  eq_object (PFixnum n P) (PFixnum m Q)
| NFixnumEq: forall n m P Q,
  n = m ->
  eq_object (NFixnum n P) (NFixnum m Q)
| Uint8Eq : forall c,
  eq_object (Uint8 c) (Uint8 c)
| Uint16Eq : forall c,
  eq_object (Uint16 c) (Uint16 c)
| Uint32Eq : forall c,
  eq_object (Uint32 c) (Uint32 c)
| Uint64Eq : forall c,
  eq_object (Uint64 c) (Uint64 c)
| Int8Eq : forall c,
  eq_object (Int8 c) (Int8 c)
| Int16Eq : forall c,
  eq_object (Int16 c) (Int16 c)
| Int32Eq : forall c,
  eq_object (Int32 c) (Int32 c)
| Int64Eq : forall c,
  eq_object (Int64 c) (Int64 c)
| FloatEq : forall c,
  eq_object (Float c) (Float c)
| DoubleEq : forall c,
  eq_object (Double c) (Double c)
| FixRawEq : forall xs,
  eq_object (FixRaw xs) (FixRaw xs)
| Raw16Eq : forall xs,
  eq_object (Raw16 xs) (Raw16 xs)
| Raw32Eq : forall xs,
  eq_object (Raw32 xs) (Raw32 xs).

Infix "=~" := eq_object (at level 60, right associativity).

Ltac apply_object_eq :=
  apply BoolEq    ||
  apply NilEq     ||
  apply PFixnumEq ||
  apply NFixnumEq ||
  apply Uint8Eq   ||
  apply Uint16Eq  ||
  apply Uint32Eq  ||
  apply Uint64Eq  ||
  apply Int8Eq    ||
  apply Int16Eq   ||
  apply Int32Eq   ||
  apply Int64Eq   ||
  apply FloatEq   ||
  apply DoubleEq  ||
  apply FixRawEq  ||
  apply Raw16Eq   ||
  apply Raw32Eq.

(* =~ が同値関係になってることの証明 *)

Lemma eq_sym : forall x,
  x =~ x.
Proof.
destruct x; apply_object_eq; auto.
Qed.

Lemma eq_refl: forall x y,
  x =~ y -> y =~ x.
Proof.
intros.
inversion H; apply_object_eq; auto.
Qed.

Lemma eq_trans: forall x y z,
  x =~ y -> y =~ z -> x =~ z.
Proof.
intros.
inversion H;
  rewrite <- H2 in *; inversion H0;
    apply_object_eq || (rewrite <- H3 in *; inversion H4);
    try inversion H5;
    try (rewrite H1, <- H3, <- H4 in *;
         inversion H5;
         reflexivity).
Qed.

Inductive Valid : object -> Prop :=
| VFixRaw : forall xs,
  length xs < pow 5 -> Valid (FixRaw xs)
| VRaw16 : forall xs,
  length xs < pow 16 -> Valid (Raw16 xs)
| VRaw32 : forall xs,
  length xs < pow 32 -> Valid (Raw32 xs).
