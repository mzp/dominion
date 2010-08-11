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


(* 記述例 *)
(*Definition cellar :=
  let x := 0 in
    And (Put (Draw (Bind x) Hand) Discard)
        (Draw (Ref x) Deck).

Eval compute in (inputs cellar nil (fun _ _ => Nop)).
*)