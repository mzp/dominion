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
  player : player list;
  board  : board;
  me     : player
}

let make_player name = {
  name;
  hands = [];
  decks = [];
  discards = []
}

let make xs =
  let players =
    List.map make_player xs in
    {
      boards = {
	play_area = [];
	supply = [];
	trash  = [];
      };
      players;

