open Base
open OUnit
open Game

let _ = begin "game.ml" >::: [
  "make_playerでプレイヤーが作れる" >:: begin fun () ->
    assert_equal {
      name = "alice";
      hands= [`Gold;`Silver];
      decks= [`Cellar];
      discards=[];
      action = 1;
      buy    = 1;
      coin   = 0 }
    @@ make_player "alice" ~hands:[`Gold;`Silver] ~decks:[`Cellar]
  end;
  "ゲーム本体がmakeで作れる" >:: begin fun () ->
    let alice =
      make_player "alice" ~hands:[`Gold] ~decks:[`Cellar] in
    let bob =
      make_player "bob" ~hands:[`Silver] ~decks:[`Cellar] in
      assert_equal {
	me = 0;
	players = [ alice; bob ];
	board = {
	  play_area = [];
	  supply = [`Moat];
	  trash  = []
	}
      } @@ make [alice;bob] [`Moat]
  end
] end +> run_test_tt_main

