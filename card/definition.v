Require Import List.

Inductive future :=
| Ref  (_ : nat)
| Lit  (_ : nat)
| Bind (_ : nat).

Inductive source :=
  Hand (* 手札 *)
| Deck (* 山札 *)
| Discard (* 捨て札 *)
| Trash (* 処分 *)
| Draw (_ : future) (_ : source) (* カードを引く *)
| Put  (_ _: source)
| And (_ _ : source).

(* ユーザの入力 *)
Inductive input :=
  Select (_ : future) (_ : source).

Fixpoint inputs (s : source) : list input :=
  match s with
  | Hand | Deck | Discard | Trash =>
      nil
  | Draw x Hand =>
    (Select x Hand)::nil
  | Draw _ _ =>
    nil
  | Put s1 s2  =>
    inputs s1 ++ inputs s2
  | And s1 s2 =>
    inputs s1 ++ inputs s2
end.


(* 記述例 *)
Definition cellar :=
  let x := 0 in
    And (Put (Draw (Bind x) Hand) Discard)
        (Draw (Ref x) Deck).

Eval compute in (inputs cellar).

