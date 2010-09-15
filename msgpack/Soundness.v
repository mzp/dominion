Require Import Object.
Require Import Serialize.

Theorem recover: forall object1 bytes object2,
  Serialized object1 bytes ->
  Serialized object2 bytes ->
  object1 =~ object2.
Proof.
intros.
inversion H;
  rewrite <- H2 in *;
  clear H2;
  inversion H0;
  apply_object_eq.

 rewrite H5,H6,H7,H8,H9,H10.
 tauto.

 rewrite H5,H6,H7,H8.
 tauto.
Qed.





