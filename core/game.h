#define act 'a = ([> `SelectFrom of \
	'a state * 'a card list * num * \
	  ((unit, 'a card list) Cc.CONT.mc -> (unit, 'b) Cc.CONT.mc) ] as 'b) Cc.prompt \
    -> [`Game of 'a t] \
    -> (unit, [> `Game of 'a t ]) Cc.CONT.mc
