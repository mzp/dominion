open Base
open Game

type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc

type 'a t = Game.t -> 'a result

let error msg _  =
  Cc.return @@ Right msg

let return value game =
  Cc.return @@ Left (value,game)

let lift = id

let bind (f : 'a t) (g : 'a -> 'b t) : 'b t = fun game ->
  Cc.bind (f game) begin fun r ->
    match r with
	Left (value, game') ->
	  g value game'
      | Right msg ->
	  Cc.return (Right msg)
  end

let left = function
    Left x -> x
  | Right _ -> failwith "must not happen"

let rec many f game =
  let open Cc in
    perform begin
      x <-- f game;
      match x with
	Left (y,game) ->
	  perform begin
	    r <-- many f game;
	    let (ys,game) = left r in
	    return @@ Left (y::ys,game)
	  end
      | Right _ ->
	  return @@ Left ([],game)
    end

let (<|>) f g game =
  let open Cc in
    perform begin
      x <-- f game;
      match x with
	  Left _ ->
	    return x
	| Right _ ->
	    g game
    end

let run game ~f =
  f game

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
let update ~f kind game =
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
