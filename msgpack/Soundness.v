Require Import Object.
Require Import Serialize.
Require Import BigEndian.

Theorem soundness: forall obj1 bytes obj2,
  Serialized obj1 bytes ->
  Serialized obj2 bytes ->
  obj1 =~ obj2.
Proof.
intros.
  inversion H;
  rewrite <- H2 in *;
  inversion H0;
  try (apply ascii8_of_16_eq in H5
        || apply ascii8_of_32_eq in H5
        || apply ascii8_of_64_eq in H5;
       rewrite H5);
  try apply_object_eq;
  try (rewrite <- H3 in *;
       inversion H5; tauto);
  try (rewrite <- H3 in *;
       inversion H6; tauto).

 rewrite H6,H7,H8,H9,H10,H11.
 tauto.

 rewrite H6,H7,H8,H9.
 tauto.
Qed.
