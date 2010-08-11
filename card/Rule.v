Inductive future :=
| Ref  (_ : nat)
| Lit  (_ : nat)
| Bind (_ : nat).

Inductive source :=
  Hand (* 手札 *)
| Deck (* 山札 *)
| Discard (* 捨て札 *)
| Draw (_ : future) (_ : source) (* カードを引く *)
| Put  (_ _: source)
| And (_ _ : source).

