(* モナド *)
type 'a t
type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc
val bind  : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val error  : string -> 'a t

(* 実行 *)
val run : Game.t -> f:'a t -> 'a result

(* 高階ルール*)
val many  : 'a t -> 'a list t
val (<|>) : 'a t -> 'a t -> 'a t

(* 基本ルール *)
val game : Game.t t
val lift : (Game.t -> 'a result) -> 'a t

type name = string

val action : name -> (int -> int) -> unit t
val buy    : name -> (int -> int) -> unit t
val coin   : name -> (int -> int) -> unit t
val draw   : name -> int -> unit t

type place = [
  `Hands of name
| `Decks of name
| `Discards of name
| `PlayArea
| `Supply
| `Trash
]
val move   : place -> place -> Game.card list -> unit t
val player : name -> (Game.player -> Game.player) -> unit t
