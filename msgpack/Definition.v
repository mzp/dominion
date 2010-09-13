Require Import Strings.Ascii.
Require Import List.

Open Local Scope char_scope.
Open Local Scope list_scope.

Inductive data :=
| Bool (_ : bool)
| Nil
| PFixnum: forall n,
  nat_of_ascii n < 128 -> data.

Definition singleton {A : Type} (x : A) := x :: nil.
Inductive Serialized : data -> list ascii -> Prop :=
| SNil  :
  Serialized Nil (singleton "192")
| STrue :
  Serialized (Bool true) (singleton "195")
| SFalse :
  Serialized (Bool false) (singleton "194")
| SPFixnum : forall x1 x2 x3 x4 x5 x6 x7 P,
  Serialized (PFixnum   (Ascii x1 x2 x3 x4 x5 x6 x7 false) P)
             (singleton (Ascii x1 x2 x3 x4 x5 x6 x7 false)).
