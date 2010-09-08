(** ドミニオンのルール記述するためのモナド。*)
(* TODO: ホントのモナドにする *)

(* 基本演算 *)
type 'a t
type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc
val bind  : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>) : 'a t -> 'b t -> 'b t
val return : 'a -> 'a t
val error  : string -> 'a t

(** stateモナド風にゲームの状態を設定できる *)
val game : Game.t t
val set_game : Game.t -> unit t

(** 実行 *)
val run : Game.t -> f:'a t -> 'a result
val run_with_tap : (Game.t -> unit) -> Game.t -> f:'a t -> 'a result

(** 基本ルール *)
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

(** 高階ルール*)
val many  : 'a t -> 'a list t
val many_ : 'a t -> unit t
val (<|>) : 'a t -> 'a t -> 'a t
val option : 'a t -> 'a option t
val fold_m : f:('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
val guard  : (Game.t -> bool) -> unit t
val filter : ('a -> bool t) -> 'a t -> 'a t
