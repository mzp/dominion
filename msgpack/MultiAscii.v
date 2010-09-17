Require Import Ascii.
Require Import BinPos.

Definition ascii8 := ascii.
Definition ascii16 : Set := (ascii * ascii)%type.
Definition ascii32 : Set := (ascii * ascii * ascii * ascii)%type.
Definition ascii64 : Set := (ascii * ascii * ascii * ascii *
                             ascii * ascii * ascii * ascii)%type.

Definition to_ascii x :=
  match x with
    | None => zero
    | Some p => ascii_of_pos p
  end.

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

Definition ascii8_of_nat :=
  ascii_of_nat.

Definition ascii16_of_nat n :=
  match n with
    | 0 => (zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 8 p),
          ascii_of_pos p)
  end.

Definition ascii32_of_nat n :=
  match n with
    | 0 => (zero,zero,zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 24 p),
          to_ascii (drop 16 p),
          to_ascii (drop 8 p),
          ascii_of_pos p)
  end.

Definition ascii64_of_nat n :=
  match n with
    | 0 => (zero,zero,zero,zero,zero,zero,zero,zero)
    | S n =>
      let p := P_of_succ_nat n in
        (to_ascii (drop 56 p),
          to_ascii (drop 48 p),
          to_ascii (drop 40 p),
          to_ascii (drop 32 p),
          to_ascii (drop 24 p),
          to_ascii (drop 16 p),
          to_ascii (drop 8 p),
          ascii_of_pos p)
  end.
