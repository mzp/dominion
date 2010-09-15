Require Import Data.
Require Import Serialize.

Theorem recover: forall data1 bytes data2,
  Serialized data1 bytes ->
  Serialized data2 bytes ->
  data1 =~ data2.
Proof.
intros.
inversion H;
  rewrite <- H2 in *;
  clear H2;
  inversion H0;
  apply_data_eq.

 rewrite H5,H6,H7,H8,H9,H10.
 tauto.

 rewrite H5,H6,H7,H8.
 tauto.
Qed.





