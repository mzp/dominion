type 'a t
type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc

val bind  : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val error  : string -> 'a t
val lift : (Game.t -> 'a result) -> 'a t

val many  : 'a t -> 'a list t
val (<|>) : 'a t -> 'a t -> 'a t

val run : Game.t -> f:'a t -> 'a result
