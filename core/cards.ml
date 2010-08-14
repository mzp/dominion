open Base
open Cc
open Game
open ExtList

type num = [
| `Const of int
| `Any ]

let selectFrom g cs num k =
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

let move src dest xs g =
  update dest ~f:(fun ys -> xs ++ ys) @@
    update src ~f:(fun ys -> ys -- xs) g

let action n =
  me ~f:(fun ({action} as p) -> { p with action = action + n })
let buy n =
  me ~f:(fun ({buy} as p) -> { p with buy = buy + n })
let coin n =
  me ~f:(fun ({coin} as p) -> { p with coin = coin + n })
let draw n =
  me ~f:(fun ({draw} as p) -> { p with draw = draw + n })

type 'a action =
    'a constraint
      'a = ([> `SelectFrom of
	  Game.t * Game.card list * num *
	    ((unit, Game.card list) Cc.CONT.mc -> (unit, 'b) Cc.CONT.mc) ]
	 as 'b) Cc.prompt -> Game.t -> (unit, [> `Game of Game.t ]) Cc.CONT.mc

let cellar p g =
  perform begin
    (xs : card list) <-- user p @@ selectFrom g g.me.hands `Any;
    let g = move `hands `discards xs g in
    (ys : card list) <-- user p @@ selectFrom g g.supply @@ `Const (List.length xs);
    ret @@ move `supply `hands ys g
  end

let market p g =
  ret @@ (g
	  +> action 1
	  +> buy    1
	  +> coin   1
	  +> draw   1)

let mine _ = assert false

let remodel _ = assert false
let smithy _ = assert false
let village _ = assert false
let woodcutter _ = assert false
let workshop _ = assert false
