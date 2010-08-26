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
      ()
    end;
    "デッキの枚数がすくないときは捨て札をデッキに戻す" >:: begin fun () ->
      ()
    end;
  ]
] end +> run_test_tt_main

