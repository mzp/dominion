Require Import List.
Require Import Ascii.
Require Import BinPos.
Require Import Wf_nat.

Definition ascii8 := ascii.
Definition ascii16 : Set := (ascii * ascii)%type.
Definition ascii32 : Set := (ascii * ascii * ascii * ascii)%type.
Definition ascii64 : Set := (ascii * ascii * ascii * ascii * ascii * ascii * ascii * ascii)%type.

(** * ascii8に落す変換 *)
Definition ascii8_of_8  (x : ascii8) :=
  x :: nil.

Definition ascii8_of_16 (p : ascii16) :=
  match p with (x1,x2) => x1::x2::nil end.
Definition ascii8_of_32 (p : ascii32) :=
  match p with (x1,x2,x3,x4) => x1::x2::x3::x4::nil end.

Definition ascii8_of_64 (p : ascii64) :=
  match p with (x1,x2,x3,x4,x5,x6,x7,x8) =>
    x1::x2::x3::x4::x5::x6::x7::x8::nil
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

Fixpoint pow (m n : nat) :=
  match n with
    | 0 =>
      1
    | (S n') =>
      m * pow m n'
  end.

Definition nat_of_ascii8 :=
  nat_of_ascii.

Definition ascii8_of_nat :=
  ascii_of_nat.

Lemma soundness_ascii8 : forall c,
  ascii8_of_nat (nat_of_ascii8 c) = c.
Proof.
apply ascii_nat_embedding.
Qed.

Definition nat_of_ascii16 bytes :=
  match bytes with
    (c1, c2) =>
    nat_of_ascii c1 * pow 2 8
    + nat_of_ascii c2
  end.

Definition ascii16_of_nat n :=
  match n with
    | 0 => (zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 8 p),
          ascii_of_pos p)
  end.

Definition nat_of_ascii32 bytes :=
  match bytes with
    (c1, c2, c3, c4) =>
    nat_of_ascii c1   * pow 2 24
    + nat_of_ascii c2 * pow 2 16
    + nat_of_ascii c3 * pow 2 8
    + nat_of_ascii c4
  end.

Definition ascii32_of_nat n :=
  match n with
    | 0 => (zero,zero,zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 24 p),
          to_ascii (drop 16 p),
          to_ascii (drop 8 p),
          ascii_of_pos p)
  end.

Definition nat_of_ascii64 bytes :=
  match bytes with
    (c1, c2, c3, c4, c5, c6, c7, c8) =>
    nat_of_ascii   c1 * pow 2 56
    + nat_of_ascii c2 * pow 2 48
    + nat_of_ascii c3 * pow 2 40
    + nat_of_ascii c4 * pow 2 32
    + nat_of_ascii c5 * pow 2 24
    + nat_of_ascii c6 * pow 2 16
    + nat_of_ascii c7 * pow 2 8
    + nat_of_ascii c8
  end.

Definition ascii64_of_nat n :=
  match n with
    | 0 => (zero,zero,zero,zero,zero,zero,zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 56 p),
          to_ascii (drop 48 p),
          to_ascii (drop 40 p),
          to_ascii (drop 32 p),
          to_ascii (drop 24 p),
          to_ascii (drop 16 p),
          to_ascii (drop 8 p),
          ascii_of_pos p)
  end.

