Require Import List.
Open Local Scope list_scope.

Definition singleton {A : Type} (x : A) :=
  x :: nil.

Definition Prefix {A} (xs ys : list A) := forall x y i,
  value x = nth_error xs i ->
  value y = nth_error ys i ->
  x = y.

Lemma prefix_hd : forall A (c1 c2 : A) xs ys,
  Prefix (c1::xs) (c2::ys) ->
  c1 = c2.
Proof.
intros.
unfold Prefix in *.
apply (H c1 c2 0);
  simpl;
  reflexivity.
Qed.

Lemma prefix_tl: forall A c (xs ys : list A),
  Prefix (c::xs) (c::ys) ->
  Prefix xs ys.
Proof.
intros.
unfold Prefix in *.
intros.
apply (H x y (S i)); simpl; tauto.
Qed.

(* duplicate *)
Lemma not_prefix_hd : forall A (c1 c2 : A) xs ys,
  c1 <> c2 ->
  ~ Prefix (c1::xs) (c2::ys).
Proof.
intros.
intro.
apply H.
unfold Prefix in H0.
apply (H0 c1 c2 0); simpl; reflexivity.
Qed.


Lemma not_prefix_tl : forall A (c : A) xs ys,
  ~ Prefix xs ys->
  ~ Prefix (c::xs) (c::ys).
Proof.
intros.
intro.
apply H.
unfold Prefix in |- *.
intros.
unfold Prefix in H0.
apply (H0 _ _ (S i)); simpl; tauto.
Qed.

Lemma not_prefix_singleton : forall A (c1 c2 : A),
  c1 <> c2 ->
  ~ Prefix (singleton c1) (singleton c2).
Proof.
intros.
unfold singleton.
apply not_prefix_hd.
tauto.
Qed.
