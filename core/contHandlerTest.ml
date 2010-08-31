open Base
open OUnit
open ContHandler
open Cc

module M = Make(struct
		  type client  = int
		  type request = int
		  type state   = int
		end)
open M

let client = 1
let state = 2

let _ = begin "contHandler.ml" >::: [
  "startで開始できる" >:: begin fun () ->
    assert_equal (Left 42) @@
      start (fun _ _  -> end_ @@ 42) client state
  end;
  "resumeで再開できる" >:: begin fun () ->
    ignore @@ start (fun suspend state -> perform begin
		       (x,_) <-- suspend (const true) state;
		       end_ @@ x+1
		     end)
      client state;
    assert_equal (Left 43) @@ resume client 42 state
  end
] end +> run_test_tt_main
