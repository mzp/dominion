open Base
open OUnit

let history = ref []

let init () =
  history := []

module R = Rules.Make(
  struct
    type t = int
    let equal = (=)
    let send ch res =
      history := (ch,res) :: !history
  end)

let assert_response res f =
  init ();
  ignore (f ());
  assert_equal ~printer:Std.dump res (List.rev !history)

let _ = begin "rules.ml" >::: [
  "JoinするとOKが返ってくる" >:: begin fun () ->
    assert_response [(42,`Ok)] begin fun () ->
      R.make "foo"
      +> R.run 42 (`Join "bar")
    end
  end;
  "SayするとJoinした人に通知される" >:: begin fun () ->
    assert_response [(42,`Ok);
		     (43,`Ok);
		     (43,`Chat ("alice","hi"));
		     (42,`Chat ("alice","hi"))] begin fun () ->
      R.make "foo"
      +> R.run 42 (`Join "alice")
      +> R.run 43 (`Join "bob")
      +> R.run 42 (`Say "hi")
    end
  end
] end +> run_test_tt_main

