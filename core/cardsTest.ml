open Base
open OUnit
open Game
open Cards
open Cc

let ok msg x y =
  assert_equal ~msg ~printer:Std.dump x y

let card name = {
  name;
  cost   = 0;
  effect = Treasure 1
}

let a () = card "A"
let b () = card "B"
let c () = card "C"
let d () = card "D"
let e () = card "E"

let assert_select name g t cs n {game;target} cards num =
  ok (name ^ " game")   g  game;
  ok (name ^ " target") t  target;
  ok (name ^ " cards")  cs cards;
  ok (name ^ " num")    n  num


let select ~check actual =
  match Cc.run actual with
      `SelectFrom (state,cs,n,k) ->
	check state cs n;
	(fun cs -> k @@ return cs)
    | _ ->
	assert false

let assert_atack name t g {game; target} =
  ok (name ^ " game")   game   g;
  ok (name ^ " target") target t

let atack ~check actual =
  match Cc.run actual with
      `AtackTo (state, k) ->
	check state;
	(fun b -> k @@ return b)
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
	~check:(assert_select "1st" game1 game1.me game1.me.hands `Any) in
    (* step 2*)
    let game2 =
      game [] [a;b] [c;d] in
    let second_step =
      select (first_step game1.me.hands)
	~check:(assert_select "2nd" game2 game2.me game2.supply (`Const 2))
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
    let f = { card "F" with effect = Victory 1 } in
    (* 手札からaを捨てる *)
    let game1 =
      game [a;b] [] [c;d;e;f] in
    let first_step =
      select (start game1 mine)
	~check:(assert_select "1st" game1 game1.me game1.me.hands (`Const 1)) in
    (* a+3以下のコストをsupplyから選ぶ *)
    let game2 =
      game [b] [a] [c;d;e;f] in
    let second_step =
      select (first_step [a])
	~check:(assert_select "2nd" game2 game2.me [c; d] (`Const 1))
    in
    (* 選んだカードが手札に加わる *)
      goal (second_step [c])
	~check:(assert_equal @@ game [c; b] [a] [d;e;f])
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
	~check:(assert_select "1st" game1 game1.me game1.me.hands (`Const 1)) in
    (* a+2以下のコストをsupplyから選ぶ *)
    let game2 =
      game [b] [a] [] [c;d;e] in
    let second_step =
      select (first_step [a])
	~check:(assert_select "2nd" game2 game2.me [c; d] (`Const 1))
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
	~check:(assert_select "1st" game1 empty_player [a; b] (`Const 1)) in
    (* 取ったカードがdiscardsに追加される *)
      goal (first_step [b])
	~check:(assert_equal @@ game [b] [a;c])
  end;
  "moat/militia" >:: begin fun () ->
    let alice =
      { empty_player with
	  player_name="alice";
	  hands=[{a () with effect=Protect }]} in
    let bob =
      { empty_player with
	  player_name="bob";
	  hands=[{a () with effect=Protect};
		 b ();
		 c ();
		 d ();
		 e ()]} in
    let charry = { empty_player with
		     player_name="charry";
		 hands = [a(); b(); c(); e()]} in
    let david =
      {empty_player with player_name="david"} in
    let game x y = {
      empty with
	me      = david;
	others = [alice; x; y ] } in
    let game1 =
      game bob charry in
    let alice_atacked =
      atack (start game1 militia)
	~check:(assert_atack "alice atacked" alice game1) in
    let bob_atacked =
      atack (alice_atacked true)
	~check:(assert_atack "bob atacked" bob game1) in
    let bob_select =
      select (bob_atacked false)
	~check:(assert_select "bob select" game1 bob bob.hands (`Const 2)) in
    let charry_select =
      select (bob_select [d (); e()])
	~check:(assert_select "charry select"
		  (game
		     { bob with
			 hands=[{a () with effect= Protect}; b (); c ()];
			 discards=[d();e()] }
		     charry)
		  charry
		  charry.hands
		  (`Const 1)) in
      goal (charry_select [a ()])
	~check:(ok "goal" @@
		  game
		  { bob with
		      hands=[{a () with effect=Protect}; b (); c ()];
		      discards=[d();e()] }
		  { charry with
		      hands =[b();c();e()];
		      discards=[a()]})
  end
] end +> run_test_tt_main
