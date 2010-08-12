open ExtList
open Base
open Cc

type card = {
  name : string;
  cost : int
}

type player = {
  hands : card list;
  decks : card list;
  discards : card list;
  action : int;
  draw   : int;
  buy    : int;
  coin   : int
}

type game = {
  me     : player;
  others : player list;
  supply : card list;
  trash  : card list
}

type ('a,'b) cont = 'a -> game -> 'b

type num = [
  `Const of int | `Any | `Range of int * int
]
type user_action = [
| `SelectFrom of game * card list * num * (card list,user_action) cont
| `Result     of game
]

let selectFrom g cs num k =
  `SelectFrom (g,cs,num,k)

let diff xs ys =
  List.fold_left (fun xs' y -> List.remove xs' y) xs ys

let compile_num compile g n k =
  match n with
      #num as x  ->
	k (x, g)
    | `All as x ->
	k (x, g)
    | `NumOf action ->
	perform begin
	  (cs, g) <-- shift @@ compile g action;
	  k (`Const (List.length cs),g)
	end

let compile_pred compile g pred k  =
  match pred with
      `Cost n ->
	k ((fun {cost} -> cost <= n), g)
    | `Only _ ->
	assert false
    | `LowCostOf (action,n) ->
	perform begin
	  (cs, g) <-- shift @@ compile g action;
	  let thres =
	    n + List.fold_left max 0 (List.map (fun{cost}->cost) cs) in
	  k ((fun {cost} -> cost <= thres ),g)
	end

let rec compile_place compile g place k =
  match place with
    | `Hands ->
	let update f =
	  { g with me = { g.me with hands = f g.me.hands } } in
	  k (g.me.hands, update)
    | `Decks ->
	let update f =
	  { g with me = { g.me with decks = f g.me.decks } } in
	  k (g.me.decks, update)
    | `Discard ->
	let update f =
	  { g with me = { g.me with discards = f g.me.discards } } in
	  k (g.me.discards, update)
    | `Supply ->
	let update f =
	  { g with supply = f g.supply }  in
	  k (g.supply, update)
    | `Trash ->
	let update f =
	  { g with trash = f g.trash } in
	  k (g.trash, update)
    | `Filter (pred, place) ->
	perform begin
	  (pred', g) <-- shift @@ compile_pred compile g pred;
	  (cs, updater) <-- shift @@ compile_place compile g place;
	  let updater' f =
	    updater begin fun cs ->
	      let order =
		List.map (fun {name} -> name) cs in
	      let (xs, ys) =
		List.partition pred' cs in
	      let zs =
		(f xs) @ ys in
		filter_map
		  (fun n ->
		     option (List.find (fun {name}->name=n)) zs)
		  order
	    end in
	    k (List.filter pred' cs, updater')
	end

let rec compile ~user g action k =
  match action with
      `Action n ->
	k ([], { g with me = { g.me with action = g.me.action + n }})
    | `Buy n ->
	k ([], { g with me = { g.me with buy  = g.me.buy + n }})
    | `Coin n ->
	k ([], { g with me = { g.me with coin = g.me.coin + n }})
    | `Draw n ->
	k ([], { g with me = { g.me with draw = g.me.draw + n }})
    | `Select {Rules.src; dest; num} ->
	perform begin
	  (cs,src') <-- shift @@ compile_place (compile ~user) g src;
	  (n,g)     <-- shift @@ compile_num (compile ~user) g num;
	  cs'   <-- shift (fun k -> user (selectFrom g cs n k));
	  let g = src' (fun xs -> diff xs cs') in
	  (_,dest') <-- shift @@ compile_place (compile ~user) g dest;
	  k (cs', dest' (fun xs -> cs' @ xs))
	end
