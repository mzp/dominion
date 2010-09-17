Require Import List.
Require Import Object.
Require Import Serialize.
Require Import BigEndian.

Open Scope list_scope.

Definition Prefix {A} (xs ys : list A) := forall x y i,
  value x = nth_error xs i ->
  value y = nth_error ys i ->
  x = y.

Lemma prefix_inv : forall A (x y : A) xs ys,
  Prefix (x::xs) (y::ys) ->
  x = y /\ Prefix xs ys.
Proof.
unfold Prefix.
split; intros.
 apply (H x y 0); simpl; reflexivity.

 apply (H x0 y0 (S i)); simpl.
  rewrite H0.
  reflexivity.

  rewrite H1.
  reflexivity.
Qed.

Lemma prefix8 : forall c1 c2,
  Prefix (ascii8_of_8 c1) (ascii8_of_8 c2) ->
  c1 = c2.
Proof.
intros.
unfold ascii8_of_8 in *.
apply prefix_inv in H.
decompose [and] H.
inversion H0.
tauto.
Qed.

Lemma prefix16 : forall c1 c2,
  Prefix (ascii8_of_16 c1) (ascii8_of_16 c2) ->
  c1 = c2.
Proof.
unfold ascii8_of_16.
intros.
destruct c1; destruct c2.
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
rewrite H0,H2.
reflexivity.
Qed.

Lemma prefix32 :forall c1 c2,
  Prefix (ascii8_of_32 c1) (ascii8_of_32 c2) ->
  c1 = c2.
Proof.
unfold ascii8_of_32.
intros.
destruct c1; destruct c2.
repeat (destruct p; destruct p0).
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
apply prefix_inv in H3.
decompose [and] H3.
apply prefix_inv in H5.
decompose [and] H5.
rewrite H0,H2,H4,H6.
reflexivity.
Qed.


Lemma prefix64 :forall c1 c2,
  Prefix (ascii8_of_64 c1) (ascii8_of_64 c2) ->
  c1 = c2.
Proof.
unfold ascii8_of_64.
intros.
destruct c1; destruct c2.
repeat (destruct p; destruct p0).
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
apply prefix_inv in H3.
decompose [and] H3.
apply prefix_inv in H5.
decompose [and] H5.
apply prefix_inv in H7.
decompose [and] H7.
apply prefix_inv in H9.
decompose [and] H9.
apply prefix_inv in H11.
decompose [and] H11.
apply prefix_inv in H13.
decompose [and] H13.
rewrite H0,H2,H4,H6,H8,H10,H12,H14.
reflexivity.
Qed.

(* 行頭符号になってないことの証明 *)
Theorem NotPrefixEncoding : forall x1 x2 y1 y2,
  ~(x1 =~ x2) ->
  Serialized x1 y1 ->
  Serialized x2 y2 ->
  ~ Prefix y1 y2.
Proof.
(* notが登場すると証明が煩雑なので、対偶をとる *)
intros.
intro.
apply H.
clear H.

inversion H0; inversion H1;
  (* 明かに等しいもの
     Eg. Bool b =~ Bool b *)
  try apply_object_eq;
  (* 明らかに異なっているもの
     Eg. Bool b =~ Nil *)
  try( rewrite <- H3, <- H5 in *;
       apply prefix_inv in H2;
       decompose [and] H2;
       (discriminate || auto));
  (* prefix8/16/32/64で証明できるもの *)
  try( apply prefix8 in H7
         || apply prefix16 in H7
         || apply prefix32 in H7
         || apply prefix64 in H7;
       rewrite H7;
       apply_object_eq).
Qed.
