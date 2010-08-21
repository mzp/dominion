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
  player : player list;
  board  : board;
  me     : player
}

val make : string list -> t
