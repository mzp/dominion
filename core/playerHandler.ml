open Base
open ListUtil
open HandlerBase
open Rule
open Game

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

let me t =
  (Game.me t#game).name

let players t =
  List.map (fun { name; _ } -> name) t#game.players

let others t =
  players t -- [ me t ]

let request t suspend name =
  let open Cc in
  let client =
    fst @@ List.find (fun (_,y)-> y = name) t#clients in
  let rec f game =
    perform begin
      (client',request) <-- suspend game;
      if client != client' then
	f game
      else
	let o =
	  match request with
	      `Select c ->
		Some c
	    | `Skip ->
		None in
	  return @@ Left(o, game)
    end in
    Rule.lift f

let of_state suspend state = object
  method me =
    me state
  method others =
    others state
  method request =
    request state suspend
end

let handle t client request =
  match t#fiber with
      Some f ->
	Fiber.resume f (client,request);
	Left (t#set_game (Fiber.value f))
    | None ->
	Right "not invoked"

let invoke t =
  let t' =
    Fiber.create begin fun suspend ->
      let open Cc in
	perform begin
	  r <-- Rule.run t#game ~f:(Turn.turn @@ of_state suspend t);
	  match r with
	      Left ((), game) ->
		Fiber.end_ game
	    | Right msg ->
		failwith msg
	end
    end
  in
    t#set_fiber (Some t')
