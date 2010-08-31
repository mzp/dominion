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

let handle k request state =
  k @@ return (request, state)

let client = 1
let state = 2

let _ = begin "contHandler.ml" >::: [
  "runで実行できる" >:: begin fun () ->
    run
      (fun p state -> perform begin
	 (x,_) <-- shiftP p (fun k -> return @@ `Cc(state, (const true, handle k)));
	 return @@ `End (x+1)
       end)
      client state;
    assert_equal (Left 43) @@ M.handle client 42 state
  end
] end +> run_test_tt_main

