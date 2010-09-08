open Base
open Game

type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc

type tap = Game.t -> unit
type 'a t = tap -> Game.t -> 'a result

let error msg _ _  =
  Cc.return @@ Right msg

let return value _ game =
  Cc.return @@ Left (value,game)

let lift (f : Game.t -> 'a result) : 'a t = fun _ game ->
  f game

let bind (f : 'a t) (g : 'a -> 'b t) : 'b t = fun tap game ->
  Cc.bind (f tap game) begin fun r ->
    match r with
	Left (value, game') ->
	  tap game';
	  g value tap game'
      | Right msg ->
	  Cc.return (Right msg)
  end

let left = function
    Left x -> x
  | Right _ -> failwith "must not happen"

let rec many (f : 'a t) : 'a list t = fun tap game ->
  let open Cc in
    perform begin
      x <-- f tap game;
      match x with
	Left (y,game) ->
	  perform begin
	    r <-- many f tap game;
	    let (ys,game) = left r in
	    return @@ Left (y::ys,game)
	  end
      | Right _ ->
	  return @@ Left ([],game)
    end

let option f tap game =
  let open Cc in
    perform begin
      x <-- f tap game;
      match x with
	  Left (y,game) ->
	    return @@ Left (Some y,game)
	| Right _ ->
	    return @@ Left (None,game)
    end

let (<|>) f g tap game =
  let open Cc in
    perform begin
      x <-- f tap game;
      match x with
	  Left _ ->
	    return x
	| Right _ ->
	    g tap game
    end

let run_with_tap tap game ~f =
  f tap game

let run game ~f =
  f ignore game

type name = string
let player name f : unit t = lift @@
  fun game ->
    Cc.return @@ Left ((),Game.update_player name ~f game)

let action name f =
  player name (fun p -> { p with action = f p.action })

let buy name f =
  player name (fun p -> { p with buy = f p.buy })

let coin name f =
  player name (fun p -> { p with coin = f p.coin })

type place = [
  `Hands of name
| `Decks of name
| `Discards of name
| `PlayArea
| `Supply
| `Trash
]
let update ~f kind = lift @@ fun game ->
  let game' =
    match kind with
	`Hands name ->
	  update_player name game ~f:(fun me -> { me with hands = f me.hands } )
      | `Decks name ->
	  update_player name game ~f:(fun me -> { me with decks = f me.decks } )
      | `Discards name ->
	  update_player name game ~f:(fun me -> { me with discards = f me.discards } )
      | `PlayArea ->
	  update_board game ~f:(fun b -> { b with play_area = f b.play_area })
      | `Supply ->
	  update_board game ~f:(fun b -> { b with supply = f b.supply })
      | `Trash ->
	  update_board game ~f:(fun b -> { b with trash = f b.trash }) in
    Cc.return (Left ((),game'))

open ListUtil
let move src dest cs =
  perform begin
    update src   ~f:(fun xs -> xs -- cs);
    update dest  ~f:(fun xs -> cs @ xs)
  end

let draw name n =
  player name begin fun p ->
    let len =
      List.length p.decks in
      if len >= n then
	{ p with
	    hands    = HList.take n p.decks @ p.hands;
	    decks    = HList.drop n p.decks }
      else
	let decks' =
	  shuffle p.discards in
	  { p with
	      discards = [];
	      hands    = p.decks @ HList.take (n - len) decks';
	      decks    = HList.drop (n - len) decks';
	  }
  end

let game = lift (fun game -> Cc.return (Left (game,game)))
let set_game game = lift (fun _ -> Cc.return (Left ((),game)))

let (>>=) = bind
let (>>) f g  = perform (f; g)

let rec fold_m ~f a =
  function
    | [] ->
	return a
    | x::xs ->
	perform (y <-- f a x;
		 fold_m ~f y xs)

let guard f =
  perform begin
    g <-- game;
    if f g then
      return ()
    else
      error ""
  end

let many_ (f : 'a t) : unit t =
  (many f) >> (return ())

let rec filter p cs =
  perform begin
    c <-- cs;
    b <-- p c;
    if b then
      return c
    else
      filter p cs
  end
