type place = [
| `Hands
| `Discard
| `Decks
| `Supply
| `Trash
]

type kind = [
  `Coin
]

type select = {
  src  : src_place;
  dest : place;
  num  : num;
} and action = [
| `Select of select
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

