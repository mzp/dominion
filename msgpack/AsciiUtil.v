Require Import Lt.
Require Import Strings.Ascii.

Lemma less_128: forall x1 x2 x3 x4 x5 x6 x7,
  nat_of_ascii (Ascii x1 x2 x3 x4 x5 x6 x7 false) < 128.
Proof.
intros.
destruct x1;
destruct x2;
destruct x3;
destruct x4;
destruct x5;
destruct x6;
destruct x7;
simpl;
repeat (apply Lt.lt_n_S);
apply Lt.lt_O_Sn.
Qed.
