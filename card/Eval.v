Require Import Rule.
Require Import SetUtil.
Require Import List.

Inductive card :=
| Victory (_ : nat)
| Treasure (_ : nat).

Record player := make_player {
  hands : list card;
  decks : list card;
  discards :list card
}.
Record game := make_game {
  me     : player;
  others : list player;
  supply : list card
}.

Definition card_dec (x y : card) : {x = y} + {x <> y}.
Proof.
repeat (decide equality).
Qed.

Inductive future :=
| Ref  (_ : nat)
| Lit  (_ : nat)
| Bind (_ : nat).

Inductive source :=
  Hand (* 手札 *)
| Deck (* 山札 *)
| Discard (* 捨て札 *).

Inductive action :=
| Source (_ : source)
| Draw (_ : future) (_ : source)
| Put  (_ : action) (_ : source)
| And (_ _ : action).

Definition cellar :=
  let x := 0 in
    And (Put (Draw (Bind x) Hand) Discard)
        (Draw (Ref x) Deck).

Definition bindings := list (nat * nat).
Definition cont A   := bindings -> list card -> player -> A.

Inductive UserAction A :=
  SelectFromHand (_ : bindings) (_ : future) (_ : cont (UserAction A))
| Nop (_ : A).

Definition resolve (bs : bindings) (x : var) :=
  match x with
    | Lit n =>
      n
    | Bound n =>
      0
    |

Fixpoint eval (bs : bindings) (act : action) (p : player) (k : cont (UserAction player)) : UserAction player :=
  match act with
    | Source Hand =>
      k bs (hands p) p
    | Source Deck =>
      k bs (decks p) p
    | Source Discard =>
      k bs (discards p) p
    | Draw x Hand =>
      SelectFromHand player bs x k
    | Draw () Deck =>
    | Put x Hand =>
      eval bs x p (fun bs' cs p' =>
        k bs' cs (make_player (cs ++ hands p')
                              (decks p')
                              (discards p')))
    | Put x Deck =>
      eval bs x p (fun bs' cs p' =>
        k bs' cs (make_player (hands p')
                              (cs ++ decks p')
                              (discards p')))
    | Put x Discard =>
      eval bs x p (fun bs' cs p' =>
        k bs' cs (make_player (hands p')
                              (decks p')
                              (cs ++ discards p')))
    | And x y =>
      eval bs x p (fun bs' _ p' =>
        eval bs' y p' (fun bs'' _ p' =>
          k bs'' nil p'))
| _ =>
  k bs nil p
  end.

(* example *)
Eval compute in eval nil cellar (make_player nil nil nil) (fun bs cs p => Nop player p).
