Require Import Ascii.
Require Import List.

Fixpoint pow (m n : nat) :=
  match n with
    | 0 =>
      1
    | (S n') =>
      m * pow m n'
  end.

Inductive object :=
| Bool (_ : bool)
| Nil
| PFixnum: forall n,
  nat_of_ascii n < 128 -> object
| NFixnum: forall n,
  (* 負の数を導入したくないので、補数表現を使う。 *)
  223 < nat_of_ascii n /\ nat_of_ascii n < 256 ->
  object
| Uint8  (_ : ascii)
| Uint16 (_ _ : ascii)
| Uint32 (_ _ _ _ : ascii)
| Uint64 (_ _ _ _ _ _ _ _ : ascii)
| Int8   (_ : ascii)
| Int16  (_ _ : ascii)
| Int32  (_ _ _ _ : ascii)
| Int64  (_ _ _ _ _ _ _ _ : ascii)
| Float  (_ _ _ _ : ascii)
| Double (_ _ _ _ _ _ _ _ : ascii)
| FixRaw: forall (xs : list ascii),
  List.length xs < 32 -> object
| Raw16: forall (xs : list ascii),
  List.length xs < pow 2 16 -> object
| Raw32: forall (xs : list ascii),
  List.length xs < pow 2 32 -> object.

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
| Uint16Eq : forall c1 c2,
  eq_object (Uint16 c1 c2) (Uint16 c1 c2)
| Uint32Eq : forall c1 c2 c3 c4,
  eq_object (Uint32 c1 c2 c3 c4) (Uint32 c1 c2 c3 c4)
| Uint64Eq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_object (Uint64 c1 c2 c3 c4 c5 c6 c7 c8) (Uint64 c1 c2 c3 c4 c5 c6 c7 c8)
| Int8Eq : forall c,
  eq_object (Int8 c) (Int8 c)
| Int16Eq : forall c1 c2,
  eq_object (Int16 c1 c2) (Int16 c1 c2)
| Int32Eq : forall c1 c2 c3 c4,
  eq_object (Int32 c1 c2 c3 c4) (Int32 c1 c2 c3 c4)
| Int64Eq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_object (Int64 c1 c2 c3 c4 c5 c6 c7 c8) (Int64 c1 c2 c3 c4 c5 c6 c7 c8)
| FloatEq : forall c1 c2 c3 c4,
  eq_object (Float c1 c2 c3 c4) (Float c1 c2 c3 c4)
| DoubleEq : forall c1 c2 c3 c4 c5 c6 c7 c8,
  eq_object (Double c1 c2 c3 c4 c5 c6 c7 c8) (Double c1 c2 c3 c4 c5 c6 c7 c8)
| FixRawEq : forall xs ys P Q,
  eq_object (FixRaw xs P) (FixRaw ys Q)
| Raw16Eq : forall xs ys P Q,
  eq_object (Raw16 xs P) (Raw16 ys Q)
| Raw32Eq : forall xs ys P Q,
  eq_object (Raw32 xs P) (Raw32 ys Q).

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
destruct x; apply_object_eq.
 reflexivity.

 reflexivity.
Qed.

Lemma eq_refl: forall x y,
  x =~ y -> y =~ x.
Proof.
intros.
inversion H; apply_object_eq.
 rewrite H0.
 reflexivity.

 rewrite H0.
 reflexivity.
Qed.

Lemma eq_trans: forall x y z,
  x =~ y -> y =~ z -> x =~ z.
Proof.
intros.
inversion H;
  rewrite <- H2 in *; inversion H0;
    apply_object_eq || (rewrite <- H3 in *; inversion H4);
    try inversion H5.

 rewrite H1, <- H3, <- H4 in *.
 inversion H5.
 reflexivity.

 rewrite H1, <- H3, <- H4 in *.
 inversion H5.
 reflexivity.
Qed.
