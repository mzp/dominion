Require Import Ascii.
Require Import BinPos.

Definition ascii8 := ascii.
Definition ascii16 : Set := (ascii * ascii)%type.
Definition ascii32 : Set := (ascii * ascii * ascii * ascii)%type.
Definition ascii64 : Set := (ascii * ascii * ascii * ascii *
                             ascii * ascii * ascii * ascii)%type.

Fixpoint drop (n :nat) (p : positive) :=
  match n with
    | 0 => Some p
    | S n' =>
      match p with
        | xI p' =>
          drop n' p'
        | xO p' =>
          drop n' p'
        | xH =>
          None
      end
  end.

