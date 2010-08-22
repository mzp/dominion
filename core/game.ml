open Base

type treasure = [
| `Gold
| `Silver
| `Copper
]

type action = [
| `Cellar
| `Market
| `Mine
| `Remodel
| `Smithy
| `Village
| `Woodcutter
| `Workshop
| `Militia
| `Moat
]

type reaction = [
| `Moat
]

type victory = [
| `Estate
| `Duchy
| `Province
| `Curse
]

type card = [
| treasure
| action
| reaction
| victory
]

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

let make_player name = {
  name;
  hands = [];
  decks = [];
  discards = []
}

let make_player name ~hands ~decks = {
  name; hands; decks;
  discards = []
}

let make players supply =
  {
    board = {
      play_area = [];
      supply = supply;
      trash  = [];
    };
    players;
    me = None;
  }

let to_string = function
  | `Gold -> "gold"
  | `Silver -> "silver"
  | `Copper -> "copper"
  | `Estate -> "estate"
  | `Duchy -> "duchy"
  | `Province -> "province"
  | `Curse -> "curse"
  | `Cellar -> "cellar"
  | `Market -> "market"
  | `Mine -> "mine"
  | `Remodel -> "remodel"
  | `Smithy -> "smithy"
  | `Village -> "village"
  | `Woodcutter -> "woodcutter"
  | `Workshop -> "workshop"
  | `Militia -> "militia"
  | `Moat -> "moat"

let is_action = function
  | #action ->
      true
  | _ ->
      false

let is_reaction = function
  | #reaction ->
      true
  | _ ->
      false
let is_treasure = function
  | #treasure ->
      true
  | _ ->
      false

let is_victory = function
  | #victory ->
      true
  | _ ->
      false

let cost _ = assert false
let coin = function
  | `Gold ->
      6
  | `Silver ->
      3
  | `Copper ->
      1
  | _ ->
      0
