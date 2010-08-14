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
    | `trash ->
	{g with trash = f g.trash }

let move src dest xs g =
  update dest ~f:(fun ys -> xs ++ ys) @@
    update src ~f:(fun ys -> ys -- xs) g

let cost n xs =
  List.filter (fun {cost} -> cost <= n) xs

let one =
  `Const 1
let two =
  `Const 2
let any =
  `Any

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
    xs <-- user p @@ selectFrom g g.me.hands any;
    let g = move `hands `discards xs g in
    ys <-- user p @@ selectFrom g g.supply @@ `Const (List.length xs);
    ret @@ move `supply `hands ys g
  end

let market _ g =
  g
  +> action 1
  +> buy    1
  +> coin   1
  +> draw   1
  +> ret

let mine p g =
  perform begin
    (* todo: コインだけを捨てるようにする *)
    [ x ] <-- user p @@ selectFrom g g.me.hands one;
    let g = move `hands `trash [x] g in
    ys <-- user p @@ selectFrom g (cost (x.cost + 3) g.supply) one;
    ret @@ move `supply `hands ys g
  end

let remodel p g =
  perform begin
    [ x ] <-- user p @@ selectFrom g g.me.hands one;
    let g = move `hands `trash [ x ] g in
    ys <-- user p @@ selectFrom g (cost (x.cost + 2) g.supply) one;
    ret @@ move `supply `discards ys g
  end

let smithy _ g =
  g
  +> draw 3
  +> ret

let village _ g =
  g
  +> action 2
  +> draw 1
  +> ret

let woodcutter _ g =
  g
  +> buy 1
  +> coin 2
  +> ret

let workshop p g =
  perform begin
    xs <-- user p @@ selectFrom g (cost 4 g.supply) one;
    ret @@ move `supply `discards xs g
  end
