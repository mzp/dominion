open Base
open OUnit
open Game
open Cards
open Cc

let ok msg x y =
  assert_equal ~msg ~printer:Std.dump x y

let gameOnly = function
    `Game _ as x ->
      x
  | _ ->
      assert false

let card name () = {
  name;
  cost = 0
}

let a () = card "A" ()
let b () = card "B" ()
let c () = card "C" ()
let d () = card "D" ()
let e () = card "E" ()


let assert_select name game cards num { current = g } cs n =
  ok (name ^ " game")  game  g;
  ok (name ^ " cards") cards cs;
  ok (name ^ " num")   num n

let select ~check actual =
  match Cc.run actual with
      `SelectFrom (state,cs,n,k) ->
	check state cs n;
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
    pushP p @@ card p @@ `Game game
  end

let _ = begin "cards.ml" >::: [
  "シナリオテスト(cellarの場合)" >:: begin fun _ ->
    let game hands discards supply = {
      empty with
	me = { empty_player with
		 hands; discards };
	supply }
    in
    let a = a () in
    let b = b () in
    let c = c () in
    let d = d () in
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
  "mineの場合" >:: begin fun _ ->
    let game hands trash supply = {
      empty with
	me = { empty_player with
		 hands };
	supply; trash }  in
    let a = a () in
    let b = b () in
    let c = c () in
    let d = { d () with cost = 3 }  in
    let e = { e () with cost = 4 } in
    (* 手札からaを捨てる *)
    let game1 =
      game [a;b] [] [c;d;e] in
    let first_step =
      select (start game1 mine)
	~check:(assert_select "1st" game1 game1.me.hands (`Const 1)) in
    (* a+3以下のコストをsupplyから選ぶ *)
    let game2 =
      game [b] [a] [c;d;e] in
    let second_step =
      select (first_step [a])
	~check:(assert_select "2nd" game2 [c; d] (`Const 1))
    in
    (* 選んだカードが手札に加わる *)
      goal (second_step [c])
	~check:(assert_equal @@ game [c; b] [a] [d;e])
  end;
  "remodelの場合" >:: begin fun _ ->
    let game hands trash discards supply = {
      empty with
	me = { empty_player with
		 hands; discards };
	supply; trash }  in
    let a = a () in
    let b = b () in
    let c = c () in
    let d = { d () with cost = 2 }  in
    let e = { e () with cost = 3 } in
    (* 手札からaを捨てる *)
    let game1 =
      game [a;b] [] [] [c;d;e] in
    let first_step =
      select (start game1 remodel)
	~check:(assert_select "1st" game1 game1.me.hands (`Const 1)) in
    (* a+2以下のコストをsupplyから選ぶ *)
    let game2 =
      game [b] [a] [] [c;d;e] in
    let second_step =
      select (first_step [a])
	~check:(assert_select "2nd" game2 [c; d] (`Const 1))
    in
    (* 選んだカードがdiscardsに加わる *)
      goal (second_step [c])
	~check:(assert_equal @@ game [b] [a] [c] [d;e])
  end;
  "smithyの場合" >:: begin fun () ->
    goal (start Game.empty smithy)
      ~check:(assert_equal {Game.empty with me = {
			      Game.empty_player with
				draw   = 3 } })
  end;
  "villageの場合" >:: begin fun () ->
    goal (start Game.empty village)
      ~check:(assert_equal {Game.empty with me = {
			      Game.empty_player with
				action = 2;
				draw   = 1 } })
  end;
  "woodcutterの場合" >:: begin fun () ->
    goal (start Game.empty woodcutter)
      ~check:(assert_equal {Game.empty with me = {
			      Game.empty_player with
				buy = 1;
				coin = 2 } })
  end;
  "workshopの場合" >:: begin fun () ->
    let game discards supply = {
      empty with
	me = { empty_player with
		 discards };
	supply }  in
    let a = a () in
    let b = { b () with cost = 4 }  in
    let c = { c () with cost = 5 } in
    (* supplyから4以下のカードを取る *)
    let game1 =
      game [] [a; b; c] in
    let first_step =
      select (start game1 workshop)
	~check:(assert_select "1st" game1 [a; b] (`Const 1)) in
    (* 取ったカードがdiscardsに追加される *)
      goal (first_step [b])
	~check:(assert_equal @@ game [b] [a;c])
  end;
] end +> run_test_tt_main
