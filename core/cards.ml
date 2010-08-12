open Rules

let cellar : effect list = [
  `Select {
      src = `Decks;
      dest = `Hands;
      num = `NumOf (`Select {
		      src  = `Hands;
		      dest = `Discard;
		      num  = `Any
		    })
  }]

let market : effect list = [
  `Action 1;
  `Buy    1;
  `Coin   1;
  `Draw   1;
]

let mine : effect list =
  [ `Select {
      src  = `Filter( `Only `Coin,
		      `Filter (`LowCostOf (`Select {
					     src  = `Hands;
					     dest = `Trash;
					     num = `Const 1
					   },3),
			       `Supply));
      dest = `Hands;
      num  = `Const 1;
    } ]

let remodel : effect list = [
  `Select {
    src = `Filter (`LowCostOf (`Select {
				 src  = `Hands;
				 dest = `Trash;
				 num  = `Const 1
			       },2),`Supply);
    dest = `Discard;
    num  = `Const 1
  } ]

let smity : effect list = [
  `Draw 3
]

let village : effect list = [
  `Action 2;
  `Draw 1;
]

let woodcutter : effect list = [
  `Buy 1;
  `Coin 2;
]

let workshop : effect list = [
  `Select {
    src  = `Filter(`Cost 4,`Supply);
    dest = `Discard;
    num  = `Const 1;
  } ]
