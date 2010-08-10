Require Import List.

Inductive source :=
  Hand (* 手札 *)
| Deck (* 山札 *)
| Discard (* 捨て札 *)
| Trash (* 処分 *)
| Draw (_ : num) (_ : source) (* カードを引く *)
with num := (* 枚数 *)
| Const (_ : nat) (* 固定 *)
| NumOf (_ : source) (* ほかのカードと同じ枚数 *)
with action :=
| Put (from to: source)
| Reveal (_ : source).