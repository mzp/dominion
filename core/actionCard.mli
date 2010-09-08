class type t = object
  method request : string -> Game.card option Rule.t
  method me : string
  method others : string list
end

val card_action : #t -> Game.card -> unit Rule.t
