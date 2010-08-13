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

type t = {
  me     : player;
  others : player list;
  supply : card list;
  trash  : card list
}

let empty_player = {
    decks=[];
    hands=[];
    discards=[];
    action=0;
    draw=0;
    buy=0;
    coin=0
}

let empty = {
  me = empty_player;
  supply=[];
  others = [];
  trash = []
}
