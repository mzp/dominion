open Base
open OUnit
open Game
open Cards

let card name cost = {
  name = name;
  cost = cost;
}

open Cc

let ok msg x y =
  assert_equal ~msg ~printer:Std.dump x y

let gameOnly = function
    `Game _ as x ->
      x
  | _ ->
      assert false

let card name = {
  name;
  cost = 0;
}

let a =
  card "A"
let b =
  card "B"
let c =
  card "C"
let d =
  card "D"

let assert_select name game cards num g cs n =
  ok (name ^ " game")  game g;
  ok (name ^ " cards") cards cs;
  ok (name ^ " num")   num n

let select ~check actual =
  match Cc.run actual with
      `SelectFrom (g,cs,n,k) ->
	check g cs n;
	(fun cs -> k @@ return cs)
    | _ ->
	assert false

let goal ~check actual =
  match Cc.run actual with
      `Game g ->
	check g
    | _ ->
	assert false

let start game card =
  perform begin
    p <-- new_prompt ();
    pushP p @@ card p game
  end

let _ = begin "cards.ml" >::: [
  "シナリオテスト(cellarの場合)" >:: begin fun _ ->
    let game hands discards supply = {
      empty with
	me = { empty_player with
		 hands; discards };
	supply }
    in
    (* step 1 *)
    let game1 =
      game [a;b] [] [c;d] in
    let first_step =
      select (start game1 cellar)
	~check:(assert_select "1st" game1 game1.me.hands `Any) in
    (* step 2*)
    let game2 =
      game [] [a;b] [c;d] in
    let second_step =
      select (first_step game1.me.hands)
	~check:(assert_select "2nd" game2 game2.supply (`Const 2))
    in
    (* step 3 *)
    let game3 =
      game [c;d] [a;b] [] in
      goal (second_step game2.supply)
	~check:(assert_equal game3)
  end;
  "シナリオテスト(marketの場合)" >:: begin fun _ ->
    goal (start Game.empty market)
      ~check:(assert_equal {Game.empty with me = {
			      Game.empty_player with
				action = 1;
				buy    = 1;
				coin   = 1;
				draw   = 1 } })
  end;
] end +> run_test_tt_main
