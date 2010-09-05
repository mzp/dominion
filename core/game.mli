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
  action : int;
  buy    : int;
  coin   : int;
}

type board = {
  play_area : card list;
  supply : card list;
  trash : card list
}

type t = {
  players : player list;
  board   : board;
  me      : int
}

val make_player : string -> hands:card list -> decks: card list -> player
val make : player list -> card list -> t
val to_string : card -> string
val of_string : string -> card
val me : t -> player
val find : t -> string -> player option

val update : f:(player -> player) -> t -> t
val update_board  : f:(board -> board) -> t -> t
val update_player : string -> f:(player->player) -> t -> t

val is_action   : card -> bool
val is_reaction : card -> bool
val is_treasure : card -> bool
val is_victory  : card -> bool

val cost : card -> int
val coin : card -> int
