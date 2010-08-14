open Base
open Cc
open Game
open ExtList

let (++) =
  (@)

let (--) xs ys =
  List.fold_left (fun xs' y -> List.remove xs' y) xs ys

let selectFrom (g : 'a Game.t) (n : Game.num) (cs : 'a Game.card list) k =
  `SelectFrom ({target=g.me; current=g}, cs, n, k)

let user p action =
  shiftP p (fun k -> return (action k))

let ret game =
  return (`Game game)

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

let treasure xs =
  List.filter (function {effect = Treasure _ } -> true | _ -> false) xs

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

let cellar p (`Game g) =
  perform begin
    xs <-- user p @@ selectFrom g any g.me.hands;
    let g = move `hands `discards xs g in
    ys <-- user p @@ selectFrom g (`Const (List.length xs)) g.supply;
    ret @@ move `supply `hands ys g
  end

let market _ (`Game g) =
  g
  +> action 1
  +> buy    1
  +> coin   1
  +> draw   1
  +> ret

let mine p (`Game g) =
  perform begin
    [ x ] <-- user p @@ selectFrom g one g.me.hands;
    let g = move `hands `trash [x] g in
    ys <-- user p @@ selectFrom g one @@ treasure @@ cost (x.cost + 3) g.supply;
    ret @@ move `supply `hands ys g
  end

let remodel p (`Game g) =
  perform begin
    [ x ] <-- user p @@ selectFrom g one g.me.hands;
    let g = move `hands `trash [ x ] g in
    ys <-- user p @@ selectFrom g one @@ cost (x.cost + 2) g.supply;
    ret @@ move `supply `discards ys g
  end

let smithy _ (`Game g) =
  g
  +> draw 3
  +> ret

let village _ (`Game g) =
  g
  +> action 2
  +> draw 1
  +> ret

let woodcutter _ (`Game g) =
  g
  +> buy 1
  +> coin 2
  +> ret

let workshop p (`Game g) =
  perform begin
    xs <-- user p @@ selectFrom g one @@ cost 4 g.supply;
    ret @@ move `supply `discards xs g
  end

let militia _ = assert false
