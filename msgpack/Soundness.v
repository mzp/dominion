Require Import Object.
Require Import Serialize.
Require Import BigEndian.

Theorem soundness: forall obj1 bytes obj2,
  Valid obj1 ->
  Valid obj2 ->
  Serialized obj1 bytes ->
  Serialized obj2 bytes ->
  obj1 =~ obj2.
Proof.
intros.
inversion H1;
rewrite <- H4 in *;
inversion H2;
  try (apply list_of_ascii16_eq in H7
        || apply list_of_ascii32_eq in H7
        || apply list_of_ascii64_eq in H7;
       rewrite H7);
  try (rewrite <- H5 in *; inversion H8);
  try apply_object_eq;
  try (rewrite <- H5 in *; discriminate).

 rewrite H8,H9,H10,H11,H12,H13.
 reflexivity.

 rewrite H8,H9,H10,H11.
 reflexivity.
Qed.

