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

let cellar p ({ me; supply } as g) =
  perform begin
    ((xs,g) : response) <-- user p (selectFrom g me.hands `Any);
    ((ys,g) : response) <-- user p (selectFrom g supply @@ `Const (List.length xs));
    ret { g with
	    supply = supply -- ys;
	    me     = { me with
			 hands    = ys ++ (me.hands -- xs);
			 discards = xs ++ me.discards } }
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
