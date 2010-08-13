open Base
open OUnit
open Game
open Cards

let card name cost = {
  name = name;
  cost = cost;
}

let take f game = function
    `hands ->
      let (xs,ys) =
	f game.me.hands in
      	(xs, {game with me = { game.me with hands = ys }})
  | `supply ->
      let (xs,ys) =
	f game.supply in
      	(xs, {game with supply = ys})

open Cc

let ok msg x y =
  assert_equal ~msg ~printer:Std.dump x y

let selectOnly = function
    `SelectFrom _ as x ->
      x
  | _ ->
      assert false

let gameOnly = function
    `Game _ as x ->
      x
  | _ ->
      assert false

let card name = {
  name;
  cost = 0;
}

let _ = begin "cards.ml" >::: [
  "シナリオテスト(cellarの場合)" >:: begin fun _ ->
    let game = {
      empty with
	me = { empty_player with
		 hands = [card "A"; card "B"] };
	supply=[card "E"; card "F"] } in
    let `SelectFrom (g,cs,num,k)  =
      selectOnly @@ run @@ perform begin
	p <-- new_prompt ();
	pushP p @@ cellar p game
       end
    in
      ok "1st game" game g;
      ok "1st cs" game.me.hands cs;
      ok "1st num" `Any num;
    let (_, game') as r =
      take (HList.splitAt 2) game `hands in
    let `SelectFrom (g,cs,num,k) =
      selectOnly @@ run @@ k (return r)
    in
      ok "2nd game" game' g;
      ok "2nd cs" game'.supply cs;
      ok "2nd hands" [] g.me.hands;
      ok "2nd num" (`Const 2) num;
    let (_, game') as r =
      take (HList.splitAt 2) g `supply in
    let `Game g =
      gameOnly @@ run @@ k (return r) in
      ok "last game" {
	empty with
	  me = { empty_player with
		   hands = [card "E"; card "F"];
		   discards = [card "A"; card "B"] }}
	g
  end
] end +> run_test_tt_main

