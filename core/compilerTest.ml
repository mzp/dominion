open Base
open OUnit
open Compiler

open Cc

let card name cost = {
  name = name;
  cost = cost;
}

let me = {
    decks=[card "A" 0; card "B" 1; card "C" 0];
    hands=[card "D" 0; card "E" 1; card "F" 0];
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

let ok expect cont =
  assert_equal expect @@ run cont

let run_compile f x =
  run @@ perform begin
    reset @@ perform (shift @@ f compile game x)
  end

let lit x =
  ok (game, x) @@ perform begin
    reset @@ perform begin
      shift @@ compile_num compile game x;
    end
  end

let _ = begin "compiler.ml" >::: [
  "自己評価式のコンパイル" >:: begin fun _ ->
    lit @@ `Const 42;
    lit @@ `Const 0;
    lit `Any;
    lit `All;
    lit @@ `Range (0,1)
  end;
  "述語のコンパイル" >::: [
    "一定コスト以下ならtrueを返す関数" >:: begin fun _ ->
      let (g, p) =
	run_compile compile_pred @@ `Cost 2 in
	assert_equal game g;
	assert_equal true  @@ p (card "x" 0);
	assert_equal true  @@ p (card "x" 1);
	assert_equal true  @@ p (card "x" 2);
	assert_equal false @@ p (card "x" 3);
	assert_equal false @@ p (card "x" 4)
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
	run_compile compile_place `Discards in
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
    end
  ];
  "アクションのコンパイル" >::: [
    "+ n Draw系" >:: begin fun _ ->
      let ok expect x =
	let (cs,g') =
	  run @@ perform begin
	    reset @@ perform (shift @@ compile game x)
	  end
	in
	  assert_equal [] cs;
	  assert_equal {game with me = expect} g' in
	ok {me with action = 2 } @@ `Action 2;
	ok {me with draw = 2 } @@ `Draw 2;
	ok {me with coin = 2 } @@ `Coin 2;
	ok {me with buy  = 2 } @@ `Buy 2;
    end
  ]
] end +> run_test_tt_main
