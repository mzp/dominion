Require Import Util.
Require Import Lists.ListSet.

Definition set A := Lists.ListSet.set A.

Definition In {A : Type} (b : A) (B : set A) : Prop :=
  set_In b B.

Definition In_dec {A : Type} (dec : x_dec A) (b : A) (B : set A) : {In b B} + {~ In b B} :=
  set_In_dec dec b B.

Definition Included {A : Type} (B C : set A) : Prop :=
  forall x, In x B -> In x C.

Definition union {A : Type} (dec : x_dec A) (B C : set A) : set A :=
  set_union dec B C.

Definition diff {A : Type} (dec : x_dec A) (B C : set A) : set A :=
  set_diff dec B C.

Definition empty {A : Type} : set A :=
  empty_set A.

Definition remove {A : Type} (dec : x_dec A) b B : set A :=
  set_remove dec b B.

Definition Disjoint {A : Type} (xs ys : set A) := forall x,
  (In x xs -> ~ In x ys) /\ (In x ys -> ~ In x xs).

Lemma union_elim: forall A (dec : x_dec A) (a : A) (B C : set A),
  In a (union dec B C) -> In a B \/ set_In a C.
Proof.
unfold In, union.
apply set_union_elim.
Qed.
