Require Import List.
Require Import Ascii.
Require Import AsciiUtil.
Require Import NArith.
Require Import Omega.
Require Import Euclid.
Require Import Recdef.


(* 右のほうに上位の桁を格納する *)
Definition ascii8 := ascii.
Definition ascii16 : Set := (ascii * ascii)%type.
Definition ascii32 : Set := (ascii * ascii * ascii * ascii)%type.
Definition ascii64 : Set := (ascii * ascii * ascii * ascii * ascii * ascii * ascii * ascii)%type.

Definition divmod (n m : nat) (P : m > 0) :=
  eucl_dev m P n.

Lemma l: 256 > 0.
omega.
Qed.

Fixpoint pow (n : nat) :=
  match n with
    | 0 =>
      1
    | (S n') =>
      2 * pow n'
  end.

Definition ascii16_of_nat (a : nat)  :=
  let (q,r,_,_) := divmod a (pow 8) l in
    (ascii_of_nat q, ascii_of_nat r).

Definition nat_of_ascii16 (a : ascii16) :=
  let (a1, a2) := a in
    (nat_of_ascii a1) * (pow 8) + (nat_of_ascii a2).

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

Lemma soundness: forall n,
  n < pow 16 ->
  n = nat_of_ascii16 (ascii16_of_nat n).
Proof.
intros.
unfold ascii16_of_nat.
destruct (divmod n (pow 8) l).
unfold nat_of_ascii16.
rewrite (nat_ascii_embedding q), (nat_ascii_embedding r).
 auto.

 simpl in g.
 omega.

 rewrite e in H.
 apply plus_elim in H.
 change (pow 16) with (pow (8+8)) in H.
 rewrite <- pow_add, mult_comm in H.
 apply mult_S_lt_reg_l in H.
  simpl in H.
  assumption.

  simpl.
  omega.
Qed.

(** * ascii8に落す変換 *)
Definition ascii8_of_8  (x : ascii8) :=
  x :: nil.

Definition ascii8_of_16 (p : ascii16) :=
  match p with
    (x1,x2) => x2::x1::nil
  end.

Definition ascii8_of_32 (p : ascii32) :=
  match p with
    (x1,x2,x3,x4) => x4::x3::x2::x1::nil
  end.

Definition ascii8_of_64 (p : ascii64) :=
  match p with
    (x1,x2,x3,x4,x5,x6,x7,x8) =>
    x8::x7::x6::x5::x4::x3::x2::x1::nil
  end.

Lemma ascii8_of_16_eq : forall c1 c2,
  ascii8_of_16 c1 = ascii8_of_16 c2 ->
  c1 = c2.
Proof.
destruct c1; destruct c2.
simpl.
intros.
inversion H.
reflexivity.
Qed.

Lemma ascii8_of_32_eq : forall c1 c2,
  ascii8_of_32 c1 = ascii8_of_32 c2 ->
  c1 = c2.
Proof.
destruct c1; destruct c2.
repeat (destruct p; destruct p0).
simpl.
intros.
inversion H.
reflexivity.
Qed.

Lemma ascii8_of_64_eq : forall c1 c2,
  ascii8_of_64 c1 = ascii8_of_64 c2 ->
  c1 = c2.
Proof.
destruct c1; destruct c2.
repeat (destruct p; destruct p0).
simpl.
intros.
inversion H.
reflexivity.
Qed.

(** * natとの相互変換 *)
(* todo: 証明 *)
Definition to_ascii x :=
  match x with
    | None => zero
    | Some p => ascii_of_pos p
  end.

Fixpoint drop (n :nat) (p : positive) :=
  match n with
    | 0 => Some p
    | S n' =>
      match p with
        | xI p' =>
          drop n' p'
        | xO p' =>
          drop n' p'
        | xH =>
          None
      end
  end.

(** * nat<=>asciiの変換 *)

Definition digits (a : ascii) :=
  let (a0, a1, a2 , a3 , a4,  a5,  a6,  a7) := a in
    a0::a1::a2::a3::a4::a5::a6::a7::nil.

(** ** ascii8 *)
Definition nat_of_ascii8 :=
  nat_of_ascii.

Definition ascii8_of_nat :=
  AsciiUtil.ascii_of_nat.

(** ** 16 bits *)

(** ** 32 bits *)
Definition ascii32_of_N (n : N) : ascii32 :=
  match n with
    | N0 => (zero, zero, zero, zero)
    | Npos p =>
      (ascii_of_pos p,
        to_ascii (drop 8 p),
        to_ascii (drop 16 p),
        to_ascii (drop 24  p))
  end.

Definition N_of_ascii32 (a : ascii32) : N :=
  let (p , p4) := a in
  let (p , p3) := p in
  let (p1, p2) := p in
    N_of_digits (digits p1 ++ digits p2 ++ digits p3 ++ digits p4).

Definition ascii32_of_nat (a : nat) : ascii32 :=
  ascii32_of_N (N_of_nat a).

Definition nat_of_ascii32 (a : ascii32) :=
  nat_of_N (N_of_ascii32 a).

(** 64bits *)
Definition ascii64_of_N (n : N) : ascii64 :=
  match n with
    | N0 => (zero, zero, zero, zero, zero, zero, zero, zero)
    | Npos p =>
      (ascii_of_pos p,
        to_ascii (drop 8  p),
        to_ascii (drop 16 p),
        to_ascii (drop 24 p),
        to_ascii (drop 32  p),
        to_ascii (drop 40 p),
        to_ascii (drop 48 p),
        to_ascii (drop 56  p))
  end.

Definition N_of_ascii64 (a : ascii64) : N :=
  let (p , p8) := a in
  let (p , p7) := p in
  let (p , p6) := p in
  let (p , p5) := p in
  let (p , p4) := p in
  let (p , p3) := p in
  let (p1, p2) := p in
    N_of_digits (digits p1 ++ digits p2 ++ digits p3 ++ digits p4 ++
                 digits p5 ++ digits p6 ++ digits p7 ++ digits p8).

Definition ascii64_of_nat (a : nat) : ascii64 :=
  ascii64_of_N (N_of_nat a).

Definition nat_of_ascii64 (a : ascii64) :=
  nat_of_N (N_of_ascii64 a).

Theorem ascii_5bits_N : forall (n : N) b1 b2 b3 b4 b5 b6 b7 b8,
  (n < 32)%N ->
  ascii_of_N n = Ascii b1 b2 b3 b4 b5 b6 b7 b8 ->
  b6 = false /\ b7 = false /\ b8 = false.
Proof.
intros.
destruct n.
 inversion H0.
 repeat split; reflexivity.

 do 8 (try destruct p);
  (* 32 <= n *)
  try (compute in H; discriminate);
  (* n < 32 *)
  try (compute in H0; inversion H0; repeat split; reflexivity).
Qed.

Theorem ascii_5bits : forall (n : nat) b1 b2 b3 b4 b5 b6 b7 b8,
  (n < 32) ->
  ascii_of_nat n = Ascii b1 b2 b3 b4 b5 b6 b7 b8 ->
  b6 = false /\ b7 = false /\ b8 = false.
Proof.
intros.
unfold nat_of_ascii, ascii_of_nat in *.
apply (ascii_5bits_N (N_of_nat n) b1 b2 b3 b4 b5); auto.
unfold Nlt.
change 32%N with (N_of_nat 32).
rewrite <- N_of_nat_compare.
rewrite <- Compare_dec.nat_compare_lt.
assumption.
Qed.
