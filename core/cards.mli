type t = [
  `Gold
| `Silver
| `Copper
| `Estate
| `Duchy
| `Province
| `Curse
| `Cellar
| `Market
| `Mine
| `Remodel
| `Smithy
| `Village
| `Woodcutter
| `Workshop
| `Militia
| `Moat
]

val make : t -> int -> 'a Game.card

(* for test *)
val cellar : 'a Game.action
val market : 'a Game.action
val mine   : 'a Game.action
val remodel: 'a Game.action
val smithy : 'a Game.action
val village : 'a Game.action
val woodcutter : 'a Game.action
val workshop : 'a Game.action
val militia : 'a Game.action

