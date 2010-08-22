type card = [
| `Gold
| `Silver
| `Copper
| `Estate
| `Duchy
| `Province
| `Curse
| `Cellar
| `Market
| `Mine
| `Remodel
| `Smithy
| `Village
| `Woodcutter
| `Workshop
| `Militia
| `Moat ]

type player = {
  name  : string;
  hands : card list;
  decks : card list;
  discards : card list;
}

type board = {
  play_area : card list;
  supply : card list;
  trash : card list
}

type t = {
  players : player list;
  board   : board;
  me      : player option
}

val make_player : string -> hands:card list -> decks: card list -> player
val make : player list -> card list -> t
val to_string : card -> string

val is_action   : card -> bool
val is_reaction : card -> bool
val is_treasure : card -> bool
val is_victory  : card -> bool

val cost : card -> int
val coin : card -> int

