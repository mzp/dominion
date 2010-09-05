open Base
open OUnit
open ContHandler
open Cc

let client = 1
let state = 2

let t =
  make ()

let _ = begin "contHandler.ml" >::: [
  "startで開始できる" >:: begin fun () ->
    assert_equal (Left 42) @@
      start t ~f:(fun _  -> end_ @@ 42)
  end;
  "resumeで再開できる" >:: begin fun () ->
    ignore @@ start t
      ~f:(fun suspend -> perform begin
	    x <-- suspend client (const true) 0;
	    end_ @@ x+1
	  end);
    assert_equal (Left 43) @@ resume t client 42
  end;
  "他のクライアントの入力も待てる" >:: begin fun () ->
    ignore @@ start t
      ~f:(fun suspend -> perform begin
	    x <-- suspend (client+1) (const true) 0;
	    end_ @@ x+1
	  end);
    assert_equal (Left 43) @@ resume t (client+1) 42
  end
] end +> run_test_tt_main
