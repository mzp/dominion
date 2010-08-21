open Base

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
