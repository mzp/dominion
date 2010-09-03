open Base

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
