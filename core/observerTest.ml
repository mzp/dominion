open Base
open OUnitUtil
open Observer

let ok expect f n =
  let source : int Observer.t =
    Observer.make () in
  let target =
    f source in
  let actual =
    ref None in
    Observer.listen target (fun x -> actual := Some x);
    __fire source n;
    assert_equal expect !actual

let _ = begin "observer.ml" >::: [
  "listenでイベントを受信できる" >:: begin fun () ->
    let target : int Observer.t =
      Observer.make () in
    let actual =
      ref None in
      Observer.listen target (fun x -> actual := Some x);
      __fire target 10;
      assert_equal (Some 10) !actual
  end;
  "clearするとイベントが受信できなくなる" >:: begin fun () ->
    let target : int Observer.t =
      Observer.make () in
    let actual =
      ref None in
      Observer.listen target (fun x -> actual := Some x);
      Observer.clear target;
      __fire target 10;
      assert_equal None !actual
  end;
  "mapするとイベントの内容が変わる" >:: begin fun () ->
    ok (Some 1) (map (fun x -> x + 1)) 0
  end;
  "filterするとイベントの消せる" >:: begin fun () ->
    ok (Some 1) (filter (fun x -> Some (x+1))) 0;
    ok None (filter (fun _ -> None)) 0
  end;
  "mergeするとイベントが混じる" >:: begin fun () ->
    let a : int Observer.t =
      Observer.make () in
    let b : string Observer.t =
      Observer.make () in
    let target =
      merge a b in
    let actual =
      ref None in
      Observer.listen target (fun x -> actual := Some x);
      __fire a 1;
      assert_equal (Some (Left 1)) !actual;
      __fire b "foo";
      assert_equal (Some (Right "foo")) !actual;
  end
] end +> run_test_xml_main

