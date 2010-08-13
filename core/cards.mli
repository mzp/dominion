type num = [
| `Const of int
| `Any ]

type 'a action =
    'a constraint
      'a = ([> `SelectFrom of
	  Game.t * Game.card list * num *
	    ((unit, Game.card list) Cc.CONT.mc -> (unit, 'b) Cc.CONT.mc) ]
	 as 'b) Cc.prompt -> Game.t -> (unit, [> `Game of Game.t ]) Cc.CONT.mc

val cellar : 'a action
val market : 'a action
