open Base
open Game
open ExtList
open Cc

let (++) =
  (@)

let (--) xs ys =
  List.fold_left (fun xs' y -> List.remove xs' y) xs ys

let selectFrom (game : 'a Game.t) target (n : Game.num) (cs : 'a Game.card list) k =
  `SelectFrom ({ game; target}, cs, n, k)

let atackTo game target k =
  `AtackTo ({ game; target },k)

let user p action =
  shiftP p (fun k -> return (action k))

let rec fold_m ~f a = function
    [] ->
      return a
  | x::xs ->
      perform begin
	b <-- f a x;
	fold_m ~f b xs
      end

let rec replace x y = function
    [] -> []
  | z::zs ->
      if x = z then
	y :: replace x y zs
      else
	z :: replace x y zs

let atacksTo p game targets ~f =
  let atack player g =
    perform begin
      (p',g) <-- f player g;
      return { g with others = replace player p' g.others }
    end in
  let has_protect player =
    List.exists
      (fun c -> c.effect = Protect)
      player.hands in
    fold_m game targets ~f:begin fun g player ->
      if has_protect player then
	perform begin
	  revealed <-- user p @@ atackTo g player;
	  if revealed then
	    return g
	  else
	    atack player g
	end
      else
	atack player g
    end

let select p g target n cards =
  user p @@ selectFrom g target n cards

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
    xs <-- select p g g.me any g.me.hands;
    let g = move `hands `discards xs g in
      ys <-- select p g g.me (`Const (List.length xs)) g.supply;
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
    [ x ] <-- select p g g.me one g.me.hands;
    let g = move `hands `trash [x] g in
      ys    <-- select p g g.me one @@ treasure @@ cost (x.cost + 3) g.supply;
      ret @@ move `supply `hands ys g
  end

let remodel p (`Game g) =
  perform begin
    [ x ] <-- select p g g.me one g.me.hands;
    let g = move `hands `trash [ x ] g in
      ys    <-- select p g g.me one @@ cost (x.cost + 2) g.supply;
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
    xs <-- select p g g.me one @@ cost 4 g.supply;
    ret @@ move `supply `discards xs g
  end

let militia p (`Game g) =
  perform begin
    g <-- atacksTo p g g.others ~f:begin fun player game ->
      let n = List.length player.hands - 3 in
	if n > 0 then
	  perform begin
	    cs <-- select p game player (`Const n) player.hands;
	    return ({ player with
			hands = player.hands -- cs;
			discards = cs ++ player.discards },
		    game)
	  end
	else
	  return (player, game)
    end;
    ret g
  end

type t = [
  `Gold
| `Silver
| `Copper
| `Estate
| `Duchy
| `Province
| `Curse
| `Cellar
| `Market
| `Mine
| `Remodel
| `Smithy
| `Village
| `Woodcutter
| `Workshop
| `Militia
| `Moat
]

let make (kind : t) id =
  let (name, cost, effect) =
    match kind with
      | `Gold ->
	  "gold", 6, Treasure 3
      | `Silver ->
	  "silver", 3, Treasure 2
      | `Copper ->
	  "copper", 0, Treasure 1
      | `Estate ->
	  "estate",2, Victory 1
      | `Duchy ->
	  "duchy", 5, Victory 3
      | `Province ->
	  "province", 8, Victory 6
      | `Curse ->
	  "curse", 0, Victory (-1)
      | `Cellar ->
	  "cellar", 2, Action cellar
      | `Moat ->
	  "moat", 2, Protect
      | `Village ->
	  "village", 3, Action village
      | `Workshop ->
	  "workshop", 3, Action workshop
      | `Woodcutter ->
	  "woodcutter", 3, Action woodcutter
      | `Smithy ->
	  "smithy", 4, Action smithy
      | `Remodel ->
	  "remodel",4,Action remodel
      | `Militia ->
	  "militia",4,Action militia
      | `Market ->
	  "market",5,Action market
      | `Mine ->
	  "mine",5,Action mine
  in
    { cost; name; effect; id }
