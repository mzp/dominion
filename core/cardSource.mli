class type t = object
  method request : string -> Game.card option Rule.t
end

val source : #t -> string -> Game.card Rule.t
val strict_source : #t -> string -> Game.card Rule.t
val supply : #t -> string -> Game.card Rule.t
val hands  : #t -> string -> Game.card Rule.t

val in_treasures : Game.card Rule.t -> Game.card Rule.t
val in_hands : string -> Game.card Rule.t -> Game.card Rule.t

val filter : (Game.card -> Game.t -> bool) -> Game.card Rule.t -> Game.card Rule.t
