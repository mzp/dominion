open Base
open Cc
open Game
open ExtList

let selectFrom (g : Game.t) (cs : card list) (num : [`Const of int | `Any]) k =
  `SelectFrom (g,cs,num,k)

let user p action =
  shiftP p (fun k -> return (action k))

let ret game =
  return (`Game game)

let (--) xs ys =
  List.fold_left (fun xs' y -> List.remove xs' y) xs ys

let (++) = (@)

let me g ~f =
  { g with me = f g.me }

let update ~f place g =
  match place with
      `discards ->
	me g ~f:fun p -> { p with discards = f p.discards }
    | `hands ->
	me g ~f:fun p -> { p with hands = f p.hands }
    | `supply ->
	{g with supply = f g.supply }

let moveTo place xs g =
  update place g ~f:(fun ys -> xs ++ ys)

let moveFrom place xs g =
  update place g ~f:(fun ys -> ys -- xs)

let cellar p g =
  perform begin
    (xs : card list) <-- user p @@ selectFrom g g.me.hands `Any;
    let g = moveTo `discards xs @@ moveFrom `hands xs g in
    (ys : card list) <-- user p @@ selectFrom g g.supply @@ `Const (List.length xs);
    ret @@ moveTo `hands ys @@ moveFrom `supply ys g
  end
