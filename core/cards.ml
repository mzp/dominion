open Rules

let cellar : effect list = [
  `Select {
      src = `Supply;
      dest = `Discard;
      num = `NumOf (`Select {
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
  [ `Select {
      src  = `Filter (`LowCostOf (`Select {
				    src = `Hands;
				    dest = `Trash;
				    num = `Const 1
				  },3),`Supply);
      dest = `Hands;
      num  = `Const 1;
    } ]

let remodel : effect list = [
  `Select {
    src = `Filter (`LowCostOf (`Select {
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
  `Select {
    src = `Filter(`Cost 4,`Supply);
    dest = `Discard;
    num = `Const 1;
  }
]

