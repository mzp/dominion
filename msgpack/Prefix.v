Require Import List.
Require Import Object.
Require Import Serialize.
Require Import BigEndian.
Require Import AsciiUtil.
Require Import Coq.omega.Omega.

Open Scope list_scope.

Definition Prefix {A} (xs ys : list A) := forall x y i,
  value x = nth_error xs i ->
  value y = nth_error ys i ->
  x = y.

Lemma prefix_eq : forall A (xs ys : list A),
  length xs = length ys ->
  Prefix xs ys ->
  xs = ys.
Proof.
unfold Prefix in *.
induction xs; induction ys; intros.
 reflexivity.

 simpl in H.
 inversion H.

 inversion H.

 simpl in H.
 inversion H.
 apply IHxs in H2.
 generalize H0.
 rewrite H2, (H0 a a0 0); simpl; auto.
 intros.
 apply (H0 _ _ (S i)); simpl; auto.
Qed.

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
  Prefix (list_of_ascii8 c1) (list_of_ascii8 c2) ->
  c1 = c2.
Proof.
intros.
unfold list_of_ascii8 in *.
apply prefix_inv in H.
decompose [and] H.
inversion H0.
tauto.
Qed.

Lemma prefix16 : forall c1 c2,
  Prefix (list_of_ascii16 c1) (list_of_ascii16 c2) ->
  c1 = c2.
Proof.
unfold list_of_ascii16.
intros.
destruct c1; destruct c2.
simpl in H.
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
rewrite H0,H2.
reflexivity.
Qed.

Lemma prefix32 :forall c1 c2,
  Prefix (list_of_ascii32 c1) (list_of_ascii32 c2) ->
  c1 = c2.
Proof.
unfold list_of_ascii32.
intros.
destruct c1; destruct c2.
destruct a; destruct a0; destruct a1; destruct a2.
simpl in H.
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
  Prefix (list_of_ascii64 c1) (list_of_ascii64 c2) ->
  c1 = c2.
Proof.
unfold list_of_ascii64.
intros.
destruct c1; destruct c2.
destruct a; destruct a0; destruct a1; destruct a2.
destruct a; destruct a3; destruct a0; destruct a4;
destruct a1; destruct a5; destruct a2; destruct a6.
simpl in H.
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

Lemma ascii8_eq_nat : forall n m,
  n < 256 ->
  m < 256 ->
  ascii8_of_nat n =  ascii8_of_nat m ->
  n = m.
Proof.
unfold ascii8_of_nat.
intros.
rewrite <- (nat_ascii_embedding n), <- (nat_ascii_embedding m); auto.
rewrite H1.
reflexivity.
Qed.

Lemma ascii16_eq_nat : forall n m,
  n < pow 16 ->
  m < pow 16 ->
  ascii16_of_nat n =  ascii16_of_nat m ->
  n = m.
Proof.
intros.
rewrite <- (nat_ascii16_embedding n), <- (nat_ascii16_embedding m); auto.
rewrite H1.
reflexivity.
Qed.

Lemma ascii32_eq_nat : forall n m,
  n < pow 32 ->
  m < pow 32 ->
  ascii32_of_nat n =  ascii32_of_nat m ->
  n = m.
Proof.
intros.
rewrite <- (nat_ascii32_embedding n), <- (nat_ascii32_embedding m); auto.
rewrite H1.
reflexivity.
Qed.

(* 行頭符号になってないことの証明 *)
Theorem NotPrefixEncoding : forall x1 x2 y1 y2,
  ~(x1 = x2) ->
  Valid x1 ->
  Valid x2 ->
  Serialized x1 y1 ->
  Serialized x2 y2 ->
  ~ Prefix y1 y2.
Proof.
(* notが登場すると証明が煩雑なので、対偶をとる *)
intros x1 x2 y1 y2 eq v1 v2 s1 s2 p.
elim eq.
clear eq.

inversion s1; inversion s2;
  try reflexivity;
  (* 明かに等しいもの
     Eg. Bool b =~ Bool b *)
  (* 明らかに異なっているもの
     Eg. Bool b =~ Nil *)
  try( rewrite <- H0, <- H2 in p ||
       rewrite <- H0, <- H3 in p ||
       rewrite <- H1, <- H3 in p ||
       rewrite <- H1, <- H4 in p ||
       rewrite <- H1, <- H5 in p;
       apply prefix_inv in p;
       decompose [and] p;
       (discriminate || auto));
  try( apply prefix8 in H4
         || apply prefix16 in H4
         || apply prefix32 in H4
         || apply prefix64 in H4;
       rewrite H4;
       reflexivity).

 inversion H3.
 reflexivity.

 inversion H3.
 reflexivity.

 cut (cs=cs0).
  intro eq.
  rewrite eq.
  reflexivity.

  rewrite <- H0 in v1.
  rewrite <- H3 in v2.
  inversion v1.
  inversion v2.
  simpl in H8.
  simpl in H10.
  apply prefix_eq; auto.
  apply ascii8_eq_nat; [omega | omega | auto ].
  rewrite <- H, <- H2.
  inversion H5.
  apply (ascii_5bits _ b1 b2 b3  b4  b5  b6  b7 b8)  in H8; auto.
  apply (ascii_5bits _ b0 b9 b10 b11 b12 b13 b14 b15) in H10; auto.
  decompose [and] H8.
  decompose [and] H10.
  rewrite H11,H17,H18,H19,H21,H22.
  reflexivity.

 cut (cs=cs0).
  intro eq.
  rewrite eq.
  reflexivity.

  rewrite <- H0 in v1.
  rewrite <- H3 in v2.
  inversion v1.
  inversion v2.
  apply prefix_inv in H6.
  decompose [and] H6.
  apply prefix_inv in H12.
  decompose [and] H12.
  apply prefix_eq; auto.
  apply ascii16_eq_nat; [omega | omega | auto ].
  rewrite <- H, <- H2.
  rewrite H11,H13.
  reflexivity.

 cut (cs = cs0).
  intro eq.
  rewrite eq.
  reflexivity.

  rewrite <- H0 in v1.
  rewrite <- H3 in v2.
  inversion v1.
  inversion v2.
  apply prefix_inv in H6.
  decompose [and] H6.
  apply prefix_inv in H12.
  decompose [and] H12.
  apply prefix_inv in H14.
  decompose [and] H14.
  apply prefix_inv in H16.
  decompose [and] H16.
  apply prefix_eq; auto.
  apply ascii32_eq_nat; try omega.
  rewrite <- H, <- H2.
  rewrite H11,H13,H15,H17.
  reflexivity.
Qed.
