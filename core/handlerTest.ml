open Base
open OUnit

(*
  ただしクライアントへのデータ送信はstubに置き換えて、
  テスト対象のモジュールを実体化する。
*)
module M = Handler.Make(struct
			  type t = int
			  let send _ _ = ()
			  let equal = (=)
			end)
open M

let _ = begin "handler.ml" >::: [
  "actionフェーズは" >::: [
    "スキップできる" >:: begin fun () ->
      ()
    end;
    "指定したカードを使える" >:: begin fun () ->
      ()
    end;
    "actionの回数だけカードを使える" >:: begin fun () ->
      ()
    end
  ];
  "buyフェーズは" >::: [
    "スキップできる" >:: begin fun () ->
      ()
    end;
    "指定したカードを買える" >:: begin fun () ->
      ()
    end;
    "buyの回数だけカードを買える" >:: begin fun () ->
      ()
    end
  ];
  "cleanupフェーズは" >::: [
    "手札を捨て札に積む" >:: begin fun () ->
      let alice =
	Game.make_player "alice"
	  ~hands:[ `Gold; `Gold; ]
	  ~decks:[ `Gold; `Silver; `Copper; `Estate; `Duchy; `Gold ] in
      let { Game.hands; decks; discards; _ } =
	Game.me @@ cleanup 5 @@ Game.make [ alice ] [] in
	assert_equal [ `Gold; `Silver; `Copper; `Estate; `Duchy ] hands;
	assert_equal [ `Gold ] decks;
	assert_equal [ `Gold; `Gold ] discards
    end;
    "デッキの枚数がすくないときは捨て札をデッキに戻す" >:: begin fun () ->
      let alice =
	Game.make_player "alice"
	  ~hands:[ ]
	  ~decks:[ `Gold ] in
      let alice =
	{ alice with
	    Game.discards = [`Silver; `Silver; `Silver; `Silver ] } in
      let { Game.hands; decks; discards; _ } =
	Game.me @@ cleanup 5 @@ Game.make [ alice ] [] in
	assert_equal ~msg:"hands" ~printer:(Std.dump $ List.map Game.to_string)[ `Gold; `Silver; `Silver; `Silver; `Silver] hands;
	assert_equal [ ] decks;
	assert_equal [ ] discards
    end;
  ]
] end +> run_test_tt_main

