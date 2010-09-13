Require Import Strings.Ascii.
Require Import List.

Open Local Scope char_scope.
Open Local Scope list_scope.

Inductive data :=
| Bool (_ : bool)
| Nil.

Definition singleton {A : Type} (x : A) := x :: nil.
Inductive Serialized : data -> list ascii -> Prop :=
| SNil  :
  Serialized Nil (singleton "192")
| STrue :
  Serialized (Bool true) (singleton "195")
| SFalse :
  Serialized (Bool false) (singleton "194").

Theorem injective : forall x1 x2 y1 y2,
  x1 <> y1 ->
  Serialized x1 x2 ->
  Serialized y1 y2 ->
  x2 <> y2.
Proof.
intros.
inversion H0; inversion H1;
  try (intro; discriminate);
  try (rewrite <- H2, <- H4 in H;
       assert False;
         [ apply H; reflexivity | contradiction ]).
Qed.
