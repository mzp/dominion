type place = [
| `Hands
| `Discard
| `Decks
| `Supply
| `Trash ]

type kind = [
| `Coin ]

type 'a num = [
| `Const of int
| `Any
| `Range of int * int
| `All
| `NumOf of 'a ]

type 'a pred = [
  `Cost of int
| `LowCostOf of 'a * int
| `Only of kind ]

type 'a src_place = [
|  place
| `Filter of 'a pred * 'a src_place ]

type 'a select = {
  src  : 'a src_place;
  dest : place;
  num  : 'a num;
}

type 'a action = [
| `Select of 'a select
]

type effect = [
| effect action
| `Action of int
| `Draw of int
| `Buy of int
| `Coin of int ]
