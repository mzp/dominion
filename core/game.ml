type card = {
  name : string;
  cost : int
}

type player = {
  hands : card list;
  decks : card list;
  discards : card list;
  action : int;
  draw   : int;
  buy    : int;
  coin   : int
}

type game = {
  me     : player;
  others : player list;
  supply : card list;
  trash  : card list
}
