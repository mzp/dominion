open Base
open OUnit
open HandlerBase

module M = ReadyHandler.Make(
  struct
    type t = int
    let send _ _ =  ()
    let equal = (=)
  end)
open M

let empty = {
  clients = [];
  ready   = [];
  playing = false;
  game    = Game.make [] []
}

let left = function
    Left x -> x
  | Right x -> failwith x

let _ = begin "redayHandler.ml" >::: [
  "メンバーが足りないうちはplayingにならない" >:: begin fun () ->
    let { playing; _ } =
      left @@ handle 42 `Ready { empty with clients = [42,"a"; 0,"b"] } in
      assert_equal playing false
  end;
  "メンバーが揃うとplayingになる" >:: begin fun () ->
    let { playing; _ } =
      left @@ handle 42 `Ready { empty with clients = [42,"a"] } in
      assert_equal playing true
  end;
] end +> run_test_tt_main

