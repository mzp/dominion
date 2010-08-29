open Base
open OUnit

let history = ref []
let reset _ =  history := []
module M = CommonHandler.Make(
  struct
    type t = int
    let send id value =
      history := (id,value):: !history
    let equal = (=)
  end)

open M
open HandlerBase

let empty = {
  clients = [];
  ready   = [];
  playing = false;
  game    = Game.make [] []
}

let response f =
  reset ();
  ignore @@ f ();
  !history

let _ = begin "commonHandler.ml" >::: [
  "join" >:: begin fun () ->
    assert_equal (Left { empty with clients = [42,"alice"]}) @@
      handle 42 (`Join "alice") empty
  end;
  "say" >:: begin fun () ->
    assert_equal [(42,`Chat ("alice","hi"))] @@ response begin fun () ->
      handle 42 (`Say "hi") { empty with clients = [42,"alice"]};
    end
  end;
  "query" >::: [
    "mine" >:: begin fun () ->
      assert_equal [42,`Cards [`Cellar]] @@ response begin fun () ->
	handle 42 (`Query `Mine)
	  { empty with
	      clients = [42,"alice"];
	      game    = Game.make [Game.make_player "alice" ~hands:[`Cellar] ~decks:[]] []
	  }
      end
    end;
    "supply" >:: begin fun () ->
      assert_equal [42,`Cards [`Cellar]] @@ response begin fun () ->
	handle 42 (`Query `Supply)
	  { empty with
	      game    = Game.make [] [`Cellar]
	  }
      end
    end
  ]
] end +> run_test_tt_main

