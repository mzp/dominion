open Base
open OUnit
open Compiler

open Cc

let card name cost = {
  name = name;
  cost = cost;
}

let me = {
    decks=[card "A" 3; card "B" 2; card "C" 4];
    hands=[card "D" 3; card "E" 1; card "F" 2];
    discards=[card "G" 0; card "H" 1; card "I" 0];
    action=0;
    draw=0;
    buy=0;
    coin=0;
}

let game = {
  me = me;
  supply=[card "J" 0; card "K" 4; card "L" 5; card "M" 10];
  others = [];
  trash = [card "O" 0; card "P" 1; card "Q" 0];
}

let action =
  `Select {
    Rules.src  = `Hands;
    dest = `Discard;
    num  = `Const 2
  }

let actioned_game= {
  game with me = {me with
		    hands = [card "E" 1];
		    discards = [card "D" 3;
				card "F" 2;
				card "G" 0;
				card "H" 1;
				card "I" 0] } }

let user = function
  | `SelectFrom (g, cs,num,k) ->
      assert_equal game g;
      assert_equal me.hands cs;
      assert_equal (`Const 2) num;
      k [card "D" 3; card "F" 2];
  | _ ->
      assert false

(* custom assertion *)
let ok expect cont =
  assert_equal expect @@ run cont

let run_cont x =
  run @@ reset @@ shift x

let run_compile f x =
  run_cont @@ f (compile ~user) game x

let lit x =
  ok (x,game) @@ reset @@
    shift @@ compile_num (compile ~user) game x

let _ = begin "compiler.ml" >::: [
  "数字のコンパイル" >::: [
    "自己評価式のコンパイル" >:: begin fun _ ->
      lit @@ `Const 42;
      lit @@ `Const 0;
      lit `Any;
      lit `All;
      lit @@ `Range (0,1)
    end;
    "アクションをともなう数字のコンパイル" >:: begin fun _ ->
      let (num,g) =
	run @@ reset @@
	  shift @@ compile_num (compile ~user) game (`NumOf action) in
	assert_equal actioned_game g;
	assert_equal (`Const 2) num
    end
  ];
  "述語のコンパイル" >::: [
    "一定コスト以下ならtrueを返す関数" >:: begin fun _ ->
      let (p, g) =
	run_compile compile_pred @@ `Cost 2 in
	assert_equal game g;
	assert_equal true  @@ p (card "x" 0);
	assert_equal true  @@ p (card "x" 1);
	assert_equal true  @@ p (card "x" 2);
	assert_equal false @@ p (card "x" 3);
	assert_equal false @@ p (card "x" 4)
    end;
    "アクションをともなう述語のテスト" >:: begin fun _ ->
      let (p, g) =
	run_compile compile_pred @@ `LowCostOf (action,3) in
	assert_equal actioned_game g;
	assert_equal true @@ p (card "x" 5);
	assert_equal true @@ p (card "x" 6);
	assert_equal false @@ p (card "x" 7);
	assert_equal false @@ p (card "x" 8)
    end
  ];
  "場所のコンパイル"  >::: [
    "手札" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place `Hands in
      let game' =
	updater (HList.drop 2) in
	assert_equal me.hands cards;
	assert_equal
	  { game with me = { me with hands = HList.drop 2 cards } }
	  game'
    end;
    "デッキ" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place `Decks in
      let game' =
	updater (HList.drop 2) in
	assert_equal ~msg:"カード" me.decks cards;
	assert_equal ~msg:"update"
	  { game with me = { me with decks = HList.drop 2 cards } }
	  game'
    end;
    "捨て札" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place `Discard in
      let game' =
	updater (HList.drop 2) in
	assert_equal ~msg:"カード" me.discards cards;
	assert_equal ~msg:"update"
	  { game with me = { me with discards = HList.drop 2 cards } }
	  game'
    end;
    "サプライ" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place `Supply in
      let game' =
	updater (HList.drop 2) in
	assert_equal game.supply cards;
	assert_equal
	  { game with supply = HList.drop 2 cards }
	  game'
    end;
    "処分" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place `Trash in
      let game' =
	updater (HList.drop 2) in
	assert_equal game.trash cards;
	assert_equal
	  { game with trash = HList.drop 2 cards }
	  game'
    end;
    "Filter(actionをともなわないやつ)" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place (`Filter (`Cost 2, `Hands)) in
      let game' =
	updater (HList.drop 1) in
	assert_equal ~msg:"card" [card "E" 1; card "F" 2] cards;
	assert_equal ~msg:"updater"
	  { game with me = { me with hands = [card "D" 3; card "F" 2] } }
	  game'
    end;
    "Filter(actionをともなうやつ" >:: begin fun _ ->
      let (cards, updater) =
	run_compile compile_place (`Filter (`LowCostOf (action,0), `Decks)) in
      let game' =
	updater (HList.drop 1) in
	assert_equal ~msg:"card" [card "A" 3; card "B" 2] cards;
	assert_equal ~msg:"updater" ~printer:Std.dump
	  { actioned_game with me = { actioned_game.me with decks = [card "B" 2; card "C" 4] } }
	  game'
    end;
  ];
  "アクションのコンパイル" >::: [
    "+ n Draw系" >:: begin fun _ ->
      let ok expect x =
	let (cs,g') =
	  run_cont @@ compile game x ~user:(fun _ -> assert false)
	in
	  assert_equal [] cs;
	  assert_equal {game with me = expect} g' in
	ok {me with action = 2 } @@ `Action 2;
	ok {me with draw = 2 } @@ `Draw 2;
	ok {me with coin = 2 } @@ `Coin 2;
	ok {me with buy  = 2 } @@ `Buy 2;
    end;
    "ユーザの入力をともなうアクション" >:: begin fun _ ->
      let (cs,g') =
	run_cont @@ compile game action ~user
      in
	assert_equal [card "D" 3; card "F" 2] cs;
	assert_equal
	  actioned_game
	  g'
    end
  ]
] end +> run_test_tt_main
