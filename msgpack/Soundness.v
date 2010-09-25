Require Import Object.
Require Import BigEndian Serialize.

Theorem soundness: forall obj1 obj2 bytes,
  Valid obj1 ->
  Valid obj2 ->
  Serialized obj1 bytes ->
  Serialized obj2 bytes ->
  obj1 = obj2.
Proof.
intros o1 o2 b v1 v2 s1 s2.
inversion s1; inversion s2;
  try (reflexivity ||
    (rewrite <- H0 in H2; inversion H2; (reflexivity || inversion H3)) ||
    (rewrite <- H0 in H3; inversion H3; (reflexivity || inversion H3)) ||
    (rewrite <- H1 in H3; inversion H3; (reflexivity || inversion H3)) ||
    (rewrite <- H1 in H4; inversion H4; (reflexivity || inversion H4)));
  rewrite <- H0 in H2;
  inversion H2;
  (apply list_of_ascii16_eq in H4 ||
   apply list_of_ascii32_eq in H4 ||
   apply list_of_ascii64_eq in H4);
  rewrite H4;
  reflexivity.
Qed.