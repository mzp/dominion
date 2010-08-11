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

Definition card := nat.
Definition bindings  := list (nat * nat).

(* ユーザの入力 *)
Definition cont A := list card -> bindings -> A.
Inductive input :=
  SelectHand (_ : future)  (_ : cont input)
| Nop.

Fixpoint inputs (s : source) (bs : bindings) (k : cont input) : input :=
  match s with
    | Hand =>
      k nil bs
    | Deck =>
      k nil bs
    | Discard =>
      k nil bs
    | Trash =>
      k nil bs
    | Draw x Hand =>
      SelectHand x (fun cs bs' => k cs (bs' ++ bs))
    | Draw x s =>
      inputs s bs (fun cs bs' => k (* draw *) cs (bs' ++ bs))
    | Put x y =>
      inputs x bs (fun from bs' =>
        inputs y (bs' ++ bs) (fun to bs'' =>
          k (* put *) nil (bs'' ++ bs' ++ bs)))
    | And x y =>
      inputs x bs (fun _ bs' =>
        inputs y (bs' ++ bs) (fun _ bs'' =>
          k nil (bs'' ++ bs' ++ bs)))
  end.

(* 記述例 *)
Definition cellar :=
  let x := 0 in
    And (Put (Draw (Bind x) Hand) Discard)
        (Draw (Ref x) Deck).


Eval compute in (inputs cellar nil (fun _ _ => Nop)).

