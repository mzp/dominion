Require Import List.
Require Import Ascii.
Require Import AsciiUtil.
Require Import NArith.
Require Import Omega.
Require Import Euclid.
Require Import Recdef.

(** 型の定義 *)
Definition ascii8 := ascii.
Definition ascii16 : Set := (ascii8  * ascii8)%type.
Definition ascii32 : Set := (ascii16 * ascii16)%type.
Definition ascii64 : Set := (ascii32 * ascii32)%type.

(** 内部定義 *)
Definition divmod (n m : nat) (P : m > 0) :=
  eucl_dev m P n.

Fixpoint pow (n : nat) :=
  match n with
    | 0 =>
      1
    | (S n') =>
      2 * pow n'
  end.

Lemma pow_lt_O : forall n,
  pow n > 0.
Proof.
induction n.
 simpl.
 omega.

 simpl.
 omega.
Qed.

(** natとの相互変換 *)
Definition nat_of_ascii8 :=
  AsciiUtil.nat_of_ascii.

Definition ascii8_of_nat :=
  AsciiUtil.ascii_of_nat.

Definition ascii16_of_nat (a : nat)  :=
  let (q,r,_,_) := divmod a (pow 8) (pow_lt_O 8) in
    (ascii8_of_nat q, ascii8_of_nat r).

Definition nat_of_ascii16 (a : ascii16) :=
  let (a1, a2) := a in
    (nat_of_ascii8 a1) * (pow 8) + (nat_of_ascii8 a2).

Definition ascii32_of_nat (a : nat)  :=
  let (q,r,_,_) := divmod a (pow 16) (pow_lt_O 16) in
    (ascii16_of_nat q, ascii16_of_nat r).

Definition nat_of_ascii32 (a : ascii32) :=
  let (a1, a2) := a in
    (nat_of_ascii16 a1) * (pow 16) + (nat_of_ascii16 a2).

Definition ascii64_of_nat (a : nat)  :=
  let (q,r,_,_) := divmod a (pow 32) (pow_lt_O 32) in
    (ascii32_of_nat q, ascii32_of_nat r).

Definition nat_of_ascii64 (a : ascii64) :=
  let (a1, a2) := a in
    (nat_of_ascii32 a1) * (pow 32) + (nat_of_ascii32 a2).

(** n < 2^mなら元にもどせることの証明 *)
Lemma mult_S_lt_reg_l :
  forall n m p, 0 < n -> n * m < n * p -> m < p.
Proof.
intros.
destruct n.
 inversion H.

elim (le_or_lt m p).
 intro.
 inversion H1.
  rewrite H2 in H0.
   elim (lt_irrefl _ H0).
   omega.

   intro.
   apply (mult_S_lt_compat_l n _ _) in H1.
   omega.
Qed.

Lemma plus_elim: forall p a b,
  a + p < b -> a < b.
Proof.
intros.
omega.
Qed.

Lemma pow_add: forall n m,
  pow n * pow m = pow (n + m).
Proof.
induction n; intros.
 simpl in *.
 omega.

 simpl.
 repeat rewrite plus_0_r.
 rewrite <- IHn, mult_plus_distr_r.
 reflexivity.
Qed.

Lemma nat_ascii8_embedding : forall n,
  n < pow 8 ->
  nat_of_ascii8 (ascii8_of_nat n) = n.
Proof.
intros.
unfold nat_of_ascii8,ascii8_of_nat.
rewrite nat_ascii_embedding.
 reflexivity.

 simpl in H.
 assumption.
Qed.

Lemma nat_ascii16_embedding : forall n,
  n < pow 16 ->
  nat_of_ascii16 (ascii16_of_nat n) = n.
Proof.
intros.
unfold ascii16_of_nat.
destruct (divmod n (pow 8) (pow_lt_O 8)).
unfold nat_of_ascii16.
rewrite (nat_ascii8_embedding q), (nat_ascii8_embedding r).
 auto.

 omega.

 rewrite e in H.
 apply plus_elim in H.
 change (pow 16) with (pow (8+8)) in H.
 rewrite <- pow_add, mult_comm in H.
 apply mult_S_lt_reg_l in H.
  assumption.

  apply pow_lt_O.
Qed.

Lemma nat_ascii32_embedding : forall n,
  n < pow 32 ->
  nat_of_ascii32 (ascii32_of_nat n) = n.
Proof.
intros.
unfold ascii32_of_nat.
destruct (divmod n (pow 16) (pow_lt_O 16)).
unfold nat_of_ascii32.
rewrite (nat_ascii16_embedding q), (nat_ascii16_embedding r).
 auto.

 omega.

 rewrite e in H.
 apply plus_elim in H.
 change (pow 32) with (pow (16+16)) in H.
 rewrite <- pow_add, mult_comm in H.
 apply mult_S_lt_reg_l in H.
  assumption.

  apply pow_lt_O.
Qed.

Lemma nat_ascii64_embedding : forall n,
  n < pow 64 ->
  nat_of_ascii64 (ascii64_of_nat n) = n.
Proof.
intros.
unfold ascii64_of_nat.
destruct (divmod n (pow 32) (pow_lt_O 32)).
unfold nat_of_ascii64.
rewrite (nat_ascii32_embedding q), (nat_ascii32_embedding r).
 auto.

 omega.

 rewrite e in H.
 apply plus_elim in H.
 change (pow 64) with (pow (32+32)) in H.
 rewrite <- pow_add, mult_comm in H.
 apply mult_S_lt_reg_l in H.
  assumption.

  apply pow_lt_O.
Qed.

(** * ascii8に落す変換 *)
Definition list_of_ascii8  (x : ascii8) :=
  x :: nil.

Definition list_of_ascii16 (p : ascii16) :=
  match p with
    (x1,x2) => (list_of_ascii8 x1) ++ (list_of_ascii8 x2)
  end.

Definition list_of_ascii32 (p : ascii32) :=
  match p with
    (x1,x2) => (list_of_ascii16 x1) ++ (list_of_ascii16 x2)
  end.

Definition list_of_ascii64 (p : ascii64) :=
  match p with
    (x1,x2) => (list_of_ascii32 x1) ++ (list_of_ascii32 x2)
  end.

Lemma list_of_ascii16_eq : forall c1 c2,
  list_of_ascii16 c1 = list_of_ascii16 c2 ->
  c1 = c2.
Proof.
destruct c1; destruct c2.
intros.
inversion H.
reflexivity.
Qed.

Lemma list_of_ascii32_eq : forall c1 c2,
  list_of_ascii32 c1 = list_of_ascii32 c2 ->
  c1 = c2.
Proof.
intros.
destruct c1; destruct c2.
destruct a; destruct a0; destruct a1; destruct a2.
intros.
inversion H.
reflexivity.
Qed.

Lemma list_of_ascii64_eq : forall c1 c2,
  list_of_ascii64 c1 = list_of_ascii64 c2 ->
  c1 = c2.
Proof.
destruct c1; destruct c2.
destruct a; destruct a0; destruct a1; destruct a2.
destruct a; destruct a3; destruct a0; destruct a4;
destruct a1; destruct a5; destruct a2; destruct a6.
simpl.
intros.
inversion H.
reflexivity.
Qed.
