open Base
open ListUtil
open Rule
open Game

type request = [
| `Select of Game.card
| `Skip
]

type 'a fiber = ( (Game.t,string) either, ('a*request)) Fiber.t
class type ['a] t = object('b)
  method fiber     : 'a fiber option
  method set_fiber : 'a fiber option -> 'b
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
  let rec f ret game =
    perform begin
      (client',request) <-- suspend (ret game);
      if client != client' then
	f (const (right "not your turn")) game
      else
	let o =
	  match request with
	      `Select c ->
		Some c
	    | `Skip ->
		None in
	  return @@ Left(o, game)
    end in
    Rule.lift (f left)

let make suspend t = object
  method me =
    me t
  method others =
    others t
  method request =
    request t suspend
end

let handle t client request =
  match t#fiber with
      Some f ->
	Fiber.resume f (client,request);
	begin match Fiber.value f with
	    Left game ->
	      Left (t#game <- game)
	  | Right _ as r ->
	      r
	end
    | None ->
	Right "not invoked"

let invoke t =
  let t' =
    Fiber.create begin fun suspend ->
      let open Cc in
	perform begin
	  r <-- Rule.run t#game ~f:(many (Turn.turn @@ make suspend t));
	  match r with
	      Left (_, game) ->
		Fiber.end_ (Left game)
	    | Right _ as r ->
		Fiber.end_ r
	end
    end
  in
    t#fiber <- Some t'
