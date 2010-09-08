open Base
open OUnit
open Fiber
open Cc

let client = 1
let state = 2

let _ = begin "contHandler.ml" >::: [
  "startで開始できる" >:: begin fun () ->
    let t =
      Fiber.create (fun _  -> end_ 42) in
	assert_equal 42 @@ value t
  end;
  "resumeで再開できる" >:: begin fun () ->
    let t =
      Fiber.create begin fun yield ->
	perform begin
	  x <-- yield 0;
	  end_ @@ x+1
	end
      end in
      resume t 0;
      assert_equal 1 @@ value t
  end
] end +> run_test_tt_main
