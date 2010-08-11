type place = [
| `Hands
| `Discard
| `Supply
| `Trash
]

type kind = [
  `Coin
]

type move = {
  src  : src_place;
  dest : place;
  num  : num;
} and action = [
| `Move of move
] and num = [
  `Const of int
| `Any
| `Range of int * int
| `All
| `NumOf of action
] and pred = [
  `Cost of int
| `LowCostOf of action * int
| `Only of kind
] and src_place = [
  place
| `Filter of pred * src_place
]

type effect = [
| action
| `Action of int
| `Draw of int
| `Buy of int
| `Coin of int
]

let cellar : effect list = [
  `Move {
      src = `Supply;
      dest = `Discard;
      num = `NumOf (`Move {
		      src  = `Hands;
		      dest = `Discard;
		      num  = `Any
		    })
   }]

let market : effect list = [
  `Action 1;
  `Draw 1;
  `Buy 1;
  `Coin 1;
]

let mine : effect list =
  [ `Move {
      src  = `Filter (`Only `Coin,`Filter (`LowCostOf (`Move {
							 src = `Hands;
							 dest = `Trash;
							 num = `Const 1
						       },3),`Supply));
      dest = `Hands;
      num  = `Const 1;
    } ]

let remodel : effect list = [
  `Move {
    src = `Filter (`LowCostOf (`Move {
				 src = `Hands;
				 dest = `Trash;
				 num = `Const 1
			       },2),`Supply);
    dest = `Discard;
    num = `Const 1
  } ]

let smity : effect list = [
  `Draw 3
]

let village : effect list = [
  `Draw 1;
  `Action 2;
]

let woodcutter : effect list = [
  `Coin 2;
  `Buy 1;
]

let workshop : effect list = [
  `Move {
    src = `Filter(`Cost 4,`Supply);
    dest = `Discard;
    num = `Const 1;
  }
]
