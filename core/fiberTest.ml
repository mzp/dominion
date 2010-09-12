open Base
open OUnitUtil
open Fiber
open Cc

let client = 1
let state = 2

let _ = begin "fiber.ml" >::: [
  "start" >:: begin fun () ->
    let t =
      Fiber.create (fun _  -> end_ 42) in
	assert_equal 42 @@ value t
  end;
  "resume" >:: begin fun () ->
    let t =
      Fiber.create begin fun yield ->
	perform begin
	  x <-- yield 0;
	  end_ @@ x+1
	end
      end in
      assert_equal true @@ is_alive t;
      resume t 0;
      assert_equal 1 @@ value t;
      assert_equal false @@ is_alive t;
  end
] end +> run_test_xml_main
