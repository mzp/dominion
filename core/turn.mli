class type t = object
  method request : string -> Game.card option Rule.t
  method me : string
  method others : string list
  method observer : Protocol.game_response Observer.t
end

val action_phase  : #t -> unit Rule.t
val buy_phase     : #t -> unit Rule.t
val cleanup_phase : #t -> unit Rule.t
val turn          : #t -> unit Rule.t
