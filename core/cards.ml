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

type response =
    card list * Game.t

let cellar p g =
  perform begin
    ((xs,g) : response) <-- user p @@ selectFrom g g.me.hands `Any;
    ((ys,g) : response) <-- user p @@ selectFrom g g.supply @@ `Const (List.length xs);
    ret { g with
	    me = { g.me with
		     hands    = ys ++ g.me.hands;
		     discards = xs ++ g.me.discards } }
  end

(*
let some_card p =
  perform begin
    x <-- shiftP p (fun k -> return (`Cont k));
    return (`Other (x+1))
  end

let _ =
  run (perform begin
	 p <-- new_prompt ();
	 r <-- pushP p (some_card p);
	 match r with
	   | `Cont k ->
	       k (return 1)
	   |_ ->
	       assert false
       end)
*)
