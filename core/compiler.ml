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
	k (g, x)
    | `All as x ->
	k (g, x)
    | `NumOf action ->
	perform begin
	  (cs,g) <-- shift @@ compile g action;
	  k (g, `Const (List.length cs))
	end

let compile_pred compile g pred k  =
  match pred with
      `Cost n ->
	k (g, fun {cost} -> cost <= n)
    | `Only _ ->
	assert false
    | `LowCostOf _ ->
	assert false

let compile_place compile g place k =
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
    | `Filter _ ->
	assert false

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
	reset @@ perform begin
	  (cs,src') <-- shift @@ compile_place (compile ~user) g src;
	  (g,n)     <-- shift @@ compile_num (compile ~user) g num;
	  cs'       <-- shift (fun k -> user (selectFrom g cs n k));
	  let g = src' (fun xs -> diff xs cs') in
	  (_,dest') <-- shift @@ compile_place (compile ~user) g dest;
	  k (cs', dest' (fun xs -> cs' @ xs))
	end

(*
let compile_pred compile_action g pred k =
  match p with
    | `Cost n ->
	k (fun { cost } -> cost <= n) g
    | `LowCostOf (action, n) ->
	compile (action :> Rules.effect) g begin fun cs g' ->
	  let thres =
	    n + List.fold_left min max_int (List.map (fun{cost}->cost) cs)
	  in
	    k (fun { cost } -> cost <= thres ) g'
	end
    | `Only _ ->
	(* todo *)
	k (fun _ -> true) g*)


(*
  match n with
    | `Const _ as x ->
	k x g
    | `Any as x ->
	k x g
    | `Range _ as x ->
	k x g
    | `All as x ->
	k x g
    | `NumOf action ->
	compile (action :> Rules.effect) g begin fun cs g' ->
	  k (`Const (List.length cs)) g'
	end*)

(*let rec compile (e : Rules.effect ) (g : game) k : user_action =
  match e with
    | `Action n ->
	k [] {g with me = {g.me with action = g.me.action + n }}
    | `Draw n ->
	k [] {g with me = {g.me with draw = g.me.draw + n }}
    | `Buy n ->
	k [] {g with me = {g.me with buy = g.me.buy + n }}
    | `Coin n ->
	k [] {g with me = {g.me with coin = g.me.coin + n }}
    | `Select { Rules.src; dest; num } ->
	compile_num num g begin fun n g1 ->
	  match n with
	    | `All ->
		compile_place src g1 begin fun (cs, update) _ ->
		  let g2 =
		    update (fun xs -> (xs,[]))
		  in
		    compile_place (dest :> Rules.src_place) g2 begin fun (_,update) _ ->
		      let g3 =
			update (fun xs -> ([], cs @ xs))
		      in
			k cs g3
		    end
		end
	    | #num as n ->
	end

and compile_place (p : Rules.src_place) (g:game) k : user_action =
  match p with
    | `Hands ->
	let update f =
	  let (xs, ys) =
	    f g.me.hands in
	    { g with me = { g.me with hands = ys} } in
	  k (g.me.hands, update) g
    | `Discard ->
	let update f =
	  let (xs, ys) =
	    f g.me.discards in
	    { g with me = { g.me with discards = ys} } in
	  k (g.me.discards, update) g
    | `Decks ->
	let update f =
	  let (xs, ys) =
	    f g.me.decks in
	    { g with me = { g.me with decks = ys} } in
	  k (g.me.decks, update) g
    | `Supply ->
	let update f =
	  let (xs, ys) =
	    f g.me.hands in
	    { g with supply = ys } in
	  k (g.supply, update) g
    | `Trash ->
	let update f =
	  let (xs, ys) =
	    f g.trash in
	    { g with trash = ys } in
	  k (g.trash, update) g
    | `Filter (pred, place) ->
	compile_pred pred g begin fun p g' ->
	  compile_place place g' begin fun (cs, update) g'' ->
	    let cs' =
	      List.filter p cs in
	    let update' f =
	      update begin fun cs ->
		let (xs, ys) =
		  List.partition p cs in
		let (taken, rest) =
		  f xs in
		  (taken, rest @ ys)
	      end in
	      k (cs',update') g''
	  end
	end

and compile_pred (p : Rules.pred) (g : game) k : user_action =
  match p with
    | `Cost n ->
	k (fun { cost } -> cost <= n) g
    | `LowCostOf (action, n) ->
	compile (action :> Rules.effect) g begin fun cs g' ->
	  let thres =
	    n + List.fold_left min max_int (List.map (fun{cost}->cost) cs)
	  in
	    k (fun { cost } -> cost <= thres ) g'
	end
    | `Only _ ->
	(* todo *)
	k (fun _ -> true) g

and compile_num (n : Rules.num) (g : game) k : user_action =
  match n with
    | `Const _ as x ->
	k x g
    | `Any as x ->
	k x g
    | `Range _ as x ->
	k x g
    | `All as x ->
	k x g
    | `NumOf action ->
	compile (action :> Rules.effect) g begin fun cs g' ->
	  k (`Const (List.length cs)) g'
	end
*)

