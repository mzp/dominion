Require Import List.
Require Import Object.
Require Import Serialize.

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

Lemma prefix8 : forall A (x1 y1 : A) tl1 tl2,
  Prefix (x1::tl1) (y1::tl2) ->
  x1 = y1.
Proof.
intros.
apply prefix_inv in H.
tauto.
Qed.

Lemma prefix16 : forall A (x1 x2 y1 y2 : A) tl1 tl2,
  Prefix (x1::x2::tl1) (y1::y2::tl2) ->
  x1 = y1 /\ x2 = y2.
Proof.
intros.
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
tauto.
Qed.

Lemma prefix32 : forall A (x1 x2 x3 x4 y1 y2 y3 y4: A) tl1 tl2,
  Prefix (x1::x2::x3::x4::tl1) (y1::y2::y3::y4::tl2) ->
  x1 = y1 /\ x2 = y2 /\ x3 = y3 /\ x4 = y4.
Proof.
intros.
apply prefix_inv in H.
decompose [and] H.
apply prefix_inv in H1.
decompose [and] H1.
apply prefix_inv in H3.
decompose [and] H3.
apply prefix_inv in H5.
decompose [and] H5.
tauto.
Qed.


Lemma prefix64 : forall A (x1 x2 x3 x4 x5 x6 x7 x8
                           y1 y2 y3 y4 y5 y6 y7 y8 : A) tl1 tl2,
  Prefix (x1::x2::x3::x4::x5::x6::x7::x8::tl1)
         (y1::y2::y3::y4::y5::y6::y7::y8::tl2) ->
         x1 = y1 /\ x2 = y2 /\ x3 = y3 /\ x4 = y4 /\
         x5 = y5 /\ x6 = y6 /\ x7 = y7 /\ x8 = y8.
Proof.
intros.
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
tauto.
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
  rewrite <- H3, <- H5 in *;
  apply prefix_inv in H2;
  decompose [and] H2;
  (discriminate || auto).

 apply prefix8 in H7.
 decompose [and] H7.
 rewrite H7.
 apply_object_eq.

 apply prefix16 in H7.
 decompose [and] H7.
 rewrite H8, H9.
 apply_object_eq.

 apply prefix32 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H12.
 apply_object_eq.

 apply prefix64 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H11,H12,H13,H14,H16.
 apply_object_eq.

 apply prefix8 in H7.
 decompose [and] H7.
 rewrite H7.
 apply_object_eq.

 apply prefix16 in H7.
 decompose [and] H7.
 rewrite H8, H9.
 apply_object_eq.

 apply prefix32 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H12.
 apply_object_eq.

 apply prefix64 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H11,H12,H13,H14,H16.
 apply_object_eq.

 apply prefix32 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H12.
 apply_object_eq.

 apply prefix64 in H7.
 decompose [and] H7.
 rewrite H8,H9,H10,H11,H12,H13,H14,H16.
 apply_object_eq.
Qed.
