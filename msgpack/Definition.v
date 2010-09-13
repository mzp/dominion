Require Import Strings.Ascii.
Inductive data :=
| nil.

Definition serialize (x : data) : ascii :=
  match x with
    | nil =>
      ascii_of_nat 192
  end.

Definition deserialize (x : ascii) : option data :=
  match nat_of_ascii x with
    | 192 => Some nil
    | _ => None
  end.

Theorem preserve : forall x,
  deserialize (serialize x) = Some x.
Proof.
induction x; intros; simpl.
 unfold deserialize.
 simpl.
 reflexivity.
Qed.

Extraction "msgpack.ml" serialize deserialize.