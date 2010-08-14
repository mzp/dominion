type num = [
| `Const of int
| `Any ]


type 'a card = {
  name : string;
  cost : int
}

and 'a player = {
  hands : 'a card list;
  decks : 'a card list;
  discards : 'a card list;
  action : int;
  draw   : int;
  buy    : int;
  coin   : int
}

and 'a t = {
  me     : 'a player;
  others : 'a player list;
  supply : 'a card list;
  trash  : 'a card list
}

and 'a state = {
  target  : 'a player;
  current : 'a t
}

and 'a action =
    'a constraint
      'a = ([> `SelectFrom of
	       'a state * 'a card list * num *
		 ((unit, 'a card list) Cc.CONT.mc -> (unit, 'b) Cc.CONT.mc) ] as 'b) Cc.prompt
    -> [`Game of 'a t]
    -> (unit, [> `Game of 'a t ]) Cc.CONT.mc

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
