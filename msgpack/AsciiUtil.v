(* -*- coding: utf-8 -*- *)
(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2010     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Contributed by Laurent Théry (INRIA);
    Adapted to Coq V8 by the Coq Development Team *)

Require Import Bool BinPos BinNat Nnat Ascii List Setoid.

(** Conversion from [N] to [ascii] *)
Definition ascii_of_N (n : N) :=
  match n with
    | N0 => zero
    | Npos p => ascii_of_pos p
  end.

(** Same for [nat] *)

Definition ascii_of_nat (a : nat) := ascii_of_N (N_of_nat a).

(** The opposite functions *)

Local Open Scope list_scope.

Fixpoint N_of_digits (l:list bool) : N :=
 match l with
  | nil => 0
  | b :: l' => (if b then 1 else 0) + 2*(N_of_digits l')
 end%N.

Definition N_of_ascii (a : ascii) : N :=
 let (a0,a1,a2,a3,a4,a5,a6,a7) := a in
 N_of_digits (a0::a1::a2::a3::a4::a5::a6::a7::nil).

Definition nat_of_ascii (a : ascii) : nat := nat_of_N (N_of_ascii a).

(** Proofs that we have indeed opposite function (below 256) *)

Theorem ascii_N_embedding :
  forall a : ascii, ascii_of_N (N_of_ascii a) = a.
Proof.
  destruct a as [[|][|][|][|][|][|][|][|]]; vm_compute; reflexivity.
Qed.

Theorem N_ascii_embedding :
  forall n:N, (n < 256)%N -> N_of_ascii (ascii_of_N n) = n.
Proof.
destruct n.
reflexivity.
do 8 (destruct p; [ | | intros; vm_compute; reflexivity ]);
 intro H; vm_compute in H; destruct p; discriminate.
Qed.

Theorem ascii_nat_embedding :
  forall a : ascii, ascii_of_nat (nat_of_ascii a) = a.
Proof.
  destruct a as [[|][|][|][|][|][|][|][|]]; compute; reflexivity.
Qed.

Theorem nat_ascii_embedding :
  forall n : nat, n < 256 -> nat_of_ascii (ascii_of_nat n) = n.
Proof.
 intros. unfold nat_of_ascii, ascii_of_nat.
 rewrite N_ascii_embedding.
 apply nat_of_N_of_nat.
 unfold Nlt.
 change 256%N with (N_of_nat 256).
 rewrite <- N_of_nat_compare.
 rewrite <- Compare_dec.nat_compare_lt. auto.
Qed.
