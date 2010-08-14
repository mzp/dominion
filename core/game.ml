type num = [
| `Const of int
| `Any ]


type card = {
  name : string;
  cost : int
}

and player = {
  hands : card list;
  decks : card list;
  discards : card list;
  action : int;
  draw   : int;
  buy    : int;
  coin   : int
}

and t = {
  me     : player;
  others : player list;
  supply : card list;
  trash  : card list
}

and state = {
  target  : player;
  current : t
}

and 'a action =
    'a constraint
      'a = ([> `SelectFrom of
	       state * card list * num *
	    ((unit, card list) Cc.CONT.mc -> (unit, 'b) Cc.CONT.mc) ] as 'b) Cc.prompt
    -> [`Game of t]
    -> (unit, [> `Game of t ]) Cc.CONT.mc

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
