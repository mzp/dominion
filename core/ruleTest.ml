open Base
open OUnit
open Rule
open Game

let inc n game =
  Game.update game ~f:(fun p -> { p with action = p.action + n})

let ret x =
  Cc.return (Left x)

let _ = begin "rule.ml" >::: [
  "適当な関数をルールにできる" >:: begin fun () ->
    let f =
      return () in
    let game =
      Game.make [] [] in
      assert_equal (Cc.run @@ Rule.run ~f game)
	(Left ((),game))
  end;
  "関数はエラーを返せる" >:: begin fun () ->
    let f  =
      error "some error" in
    let game =
      Game.make [] [] in
      assert_equal (Cc.run @@ Rule.run ~f game)
	(Right "some error")
  end;
  "performでaction同士をつなげる" >:: begin fun () ->
    let f n =
      lift (fun game -> ret (n, inc n game))
    in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run game ~f:perform begin
	x <-- f 1;
	y <-- f 2;
	Rule.return (x+y)
      end in
      assert_equal (Left (3,game)) result
  end;
  "途中でエラーがでたら中断する" >:: begin fun () ->
    let f =
      error "some error" in
    let g =
      return () in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run game ~f:perform begin
	_ <-- f;
	_ <-- g;
	Rule.return (42,game)
      end in
      assert_equal (Right "some error") result
  end;
  "エラーを返すまで繰替えす" >:: begin fun () ->
    let f =
      lift @@ fun game ->
	let { action; _ } =
	  Game.me game in
	  if action = 1 then
	    Cc.return (Right "")
	  else
	    ret (action, inc (-1) game) in
    let game =
      Game.make [Game.make_player "alice" ~hands:[] ~decks:[]] [] in
    let result =
      Cc.run @@ Rule.run (inc 3 game) ~f:(many f) in
      assert_equal (Left ([4;3;2], game)) result
  end;
  "エラーを返さなかったらSomeになる" >:: begin fun () ->
    let f =
      return 42 in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run game ~f:(option f) in
      assert_equal (Left (Some 42,game)) result
  end;
  "エラーを返したらNoneになる" >:: begin fun () ->
    let f =
      error "" in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run game ~f:(option f) in
      assert_equal (Left (None,game)) result
  end;
  "エラーを返さないほうを採用する" >:: begin fun () ->
    let f =
      error "" in
    let g =
      return 42 in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run game ~f:(f <|> g) in
      assert_equal (Left (42, game)) result
  end;
  "actionの変更" >:: begin fun () ->
    let f =
      action "alice" ((+) 3) in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[] in
    let game =
      Game.make [ alice ] [] in
    let game' =
      Game.make [ { alice with action = 4 } ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "buyの変更" >:: begin fun () ->
    let f =
      buy "alice" ((+) 3) in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[] in
    let game =
      Game.make [ alice ] [] in
    let game' =
      Game.make [ { alice with buy = 4 } ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "coinの変更" >:: begin fun () ->
    let f =
      Rule.coin "alice" ((+) 3) in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[] in
    let game =
      Game.make [ alice ] [] in
    let game' =
      Game.make [ { alice with coin = 3 } ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "drawはdeckをhandに移動させる" >:: begin fun () ->
    let f =
      Rule.draw "alice" 2 in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[`Gold; `Silver; `Copper] in
    let alice' =
      Game.make_player "alice" ~hands:[`Gold; `Silver] ~decks:[`Copper] in
    let game =
      Game.make [ alice ] [] in
    let game' =
      Game.make [ alice' ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "drawはdeckが足りない場合はdiscardsを再利用する" >:: begin fun () ->
    let f =
      Rule.draw "alice" 1 in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[] in
    let alice' =
      Game.make_player "alice" ~hands:[`Gold] ~decks:[] in
    let game =
      Game.make [ { alice with discards=[`Gold] } ] [] in
    let game' =
      Game.make [ alice' ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "moveによるカードの移動" >:: begin fun () ->
    let f =
      perform begin
	Rule.move `Supply (`Hands "alice") [`Gold];
	Rule.move `Supply (`Decks "alice") [`Copper]
      end in
    let alice =
      Game.make_player "alice" ~hands:[] ~decks:[] in
    let alice' =
      Game.make_player "alice" ~hands:[`Gold] ~decks:[`Copper] in
    let game =
      Game.make [ alice ] [ `Gold; `Copper] in
    let game' =
      Game.make [ alice' ] [] in
    let result =
      Cc.run @@ Rule.run game ~f  in
      assert_equal (Left ((),game')) result
  end;
  "gameの差し替え">:: begin fun () ->
    let f =
      perform begin
	g <-- game;
	set_game ({g with me = 1})
      end in
    let game =
      Game.make [] [] in
      assert_equal (Cc.run @@ Rule.run ~f game)
	(Left ((),{ game with me = 1}))
  end;
  "適当な関数をはさみこめる" >:: begin fun () ->
    let count =
      ref 0 in
    let tap _ =
      count := !count + 1 in
    let f n =
      lift (fun game -> ret (n, inc n game))
    in
    let game =
      Game.make [] [] in
    let result =
      Cc.run @@ Rule.run_with_tap tap game ~f:perform begin
	x <-- f 1;
	y <-- f 2;
	Rule.return (x+y)
      end in
      assert_equal (Left (3,game)) result;
      assert_equal ~printer:Std.dump 2 !count
  end
] end +> run_test_tt_main
