Require Import List.
Require Import Data.
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
  try apply_data_eq;
  (* 明らかに異なっているもの
     Eg. Bool b =~ Nil *)
  rewrite <- H3, <- H5 in *;
  apply prefix_inv in H2;
  decompose [and] H2;
  (discriminate || auto).

 apply prefix_inv in H7.
 decompose [and] H7.
 rewrite H8.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 rewrite H8, H10.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 rewrite H8,H10,H12,H14.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 apply prefix_inv in H15.
 decompose [and] H15.
 apply prefix_inv in H17.
 decompose [and] H17.
 apply prefix_inv in H19.
 decompose [and] H19.
 apply prefix_inv in H21.
 decompose [and] H21.
 rewrite H8,H10,H12,H14,H16,H18,H20,H22.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 rewrite H8.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 rewrite H8, H10.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 rewrite H8,H10,H12,H14.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 apply prefix_inv in H15.
 decompose [and] H15.
 apply prefix_inv in H17.
 decompose [and] H17.
 apply prefix_inv in H19.
 decompose [and] H19.
 apply prefix_inv in H21.
 decompose [and] H21.
 rewrite H8,H10,H12,H14,H16,H18,H20,H22.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 rewrite H8,H10,H12,H14.
 apply_data_eq.

 apply prefix_inv in H7.
 decompose [and] H7.
 apply prefix_inv in H9.
 decompose [and] H9.
 apply prefix_inv in H11.
 decompose [and] H11.
 apply prefix_inv in H13.
 decompose [and] H13.
 apply prefix_inv in H15.
 decompose [and] H15.
 apply prefix_inv in H17.
 decompose [and] H17.
 apply prefix_inv in H19.
 decompose [and] H19.
 apply prefix_inv in H21.
 decompose [and] H21.
 rewrite H8,H10,H12,H14,H16,H18,H20,H22.
 apply_data_eq.
Qed.

