type request = [
| `Select of Game.card
| `Skip
]


class type ['a] t = object('b)
  method fiber     : (Game.t, ('a*request)) Fiber.t option
  method set_fiber : (Game.t, ('a*request)) Fiber.t option -> 'b
  method observer  : Game.t Observer.t
  method game      : Game.t
  method set_game  : Game.t -> 'b
  method clients   : ('a * string) list
end

val invoke : ('a #t as 'b) -> 'b
val handle : ('a #t as 'b) -> 'a -> request -> ('b,string) Base.either
