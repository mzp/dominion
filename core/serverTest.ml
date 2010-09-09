(* ゲーム全体がちゃんとうごくかどうかのシステムテスト。  *)
open Base
open OUnit
open Ccell

ListUtil.no_shuffle := true

let table : (string, unit Protocol.peer) Hashtbl.t = Hashtbl.create 10

module S  = struct
  type t = unit
  let rec connect host port =
    try
      Hashtbl.find table host
    with Not_found ->
      Thread.yield ();
      connect host port

  let server host _ ~f =
    let req =
      Event.new_channel () in
    let res =
      Event.new_channel () in
    let peer =
      { Protocol.req; res; id = () } in
      Hashtbl.add table host peer;
      while true do
	f { Protocol.req; res; id = () }
      done
end

module M = Server.Make(S)

let rec wait_for ({ Protocol.res = ch ; _ } as t) id =
  let res =
    Event.sync @@ Event.receive ch in
    match res with
      | `Ok id' | `Error (id',_) | `Cards (id',_) | `Games (id',_) when id = id' ->
	  res
      | _ ->
	  wait_for t id

let send { Protocol.req; _ } x =
  Event.sync @@ Event.send req x

let game x =
 `Game("foo", `Query("id", x))

let ok t res =
  assert_equal ~printer:Std.dump res @@ wait_for t "id"

let _ = begin "server.ml" >::: [
  "Listで作成したゲームした一覧が取得できる" >:: begin fun () ->
    let _ =
      Thread.create (fun _ -> M.run "some-server" 1729) () in
    let c1 =
      S.connect "some-server" 1729 in
    let c2 =
      S.connect "some-server" 1729 in
      send c1 @@ `List "id";
      ok   c1 @@ `Games ("id",[]);
      send c2 @@ `Make ("id","foo");
      ok   c2 @@ `Ok "id";
      send c1 @@ `List "id";
      ok   c1 @@ `Games ("id",["foo"]);
      send c1 @@ `List "id";
      ok   c2 @@ `Games ("id",["foo"]);
  end;
  "joinしてゲーム開始ができる" >:: begin fun () ->
    let _ =
      Thread.create (fun _ -> M.run "some-server" 1729) () in
    let c1 =
      S.connect "some-server" 1729 in
    let c2 =
      S.connect "some-server" 1729 in
      (* 部屋の作成 *)
      send c2 @@ `Make ("id","foo");
      ok   c2 @@ `Ok "id";
      (* join *)
      send c1 @@ game @@ `Join "alice";
      ok   c1 @@ `Ok "id";
      send c2 @@ game @@ `Join "bob";
      ok   c2 @@ `Ok "id";
      (* ready *)
      send c1 @@ game @@ `Ready;
      ok   c1 @@ `Ok "id";
      send c2 @@ game @@ `Ready;
      ok   c2 @@ `Ok "id";
  end
] end +> run_test_tt_main
