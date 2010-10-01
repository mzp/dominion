(* ゲーム全体がちゃんとうごくかどうかのシステムテスト。  *)
open Base
open OUnitUtil
open Ccell

ListUtil.no_shuffle := true;;

Logger.set_level Logger.Never;;

let table = Hashtbl.create 10

module S  = struct
  type t = int
  let rec connect host _ =
    let req =
      Event.new_channel () in
    let res =
      Event.new_channel ()  in
    let peer =
      { Protocol.req; res; id = Std.unique() } in
    let f =
      Hashtbl.find table host in
      ignore @@ Thread.create (fun () ->
				 while true do
				   f peer
				 done)
	();
      peer

  let server host _ ~f =
      Hashtbl.add table host f
end

module M = Server.Make(S)

let history =
  ref []

let rec wait_for ({ Protocol.res = ch ; _ } as t) id =
  let res =
    Event.sync @@ Event.receive ch in
    match res with
      | `Ok id' | `Error (id',_) | `Cards (id',_) | `Games (id',_) when id = id' ->
	  res
      | _ ->
	  history := res :: ! history;
	  wait_for t id

let assert_mem x =
  assert_equal true @@ List.mem x !history

let t =
  Timeout.make 0.1

let rec assert_recv count expect c =
  if count = 0 then
    assert false
  else
    match Event.sync (Timeout.with_timeout t 0.5 (Event.receive c.Protocol.res)) with
      | None -> assert false
      | Some v ->
	  if expect = v then
	    ()
	  else
	    assert_recv (count-1) expect c

let assert_recv expect c =
  if List.mem expect !history then
    ()
  else
    assert_recv 10 expect c

let last_id = ref ""
let n = ref 0

let id () =
  last_id :=  string_of_int @@ !n;
  incr n;
  !last_id

let get_last_id () =
  !last_id

let send { Protocol.req; _ } x =
  Event.sync @@ Event.send req x

let recv { Protocol.res; _ } =
  Event.sync @@ Event.receive res

let game_name =
  "foo"

let game x =
 `Game(game_name, `Query(id (), x))

let message x =
  `Message(game_name,x)

let ok t res =
  assert_equal ~printer:Std.dump res @@ wait_for t (get_last_id())

let start _ =
    let _ =
      history := [] in
    let _ =
      M.run "join" 1729 in
    let c1 =
      S.connect "join" 1729 in
    let c2 =
      S.connect "join" 1729 in
      (* 部屋の作成 *)
      send c2 @@ `Make (id (),"foo");
      ok   c2 @@ `Ok (get_last_id());
      (* join *)
      send c1 @@ game @@ `Join "alice";
      ok   c1 @@ `Ok (get_last_id());
      send c2 @@ game @@ `Join "bob";
      ok   c2 @@ `Ok (get_last_id());
      (* ready *)
      send c1 @@ game @@ `Ready;
      ok   c1 @@ `Ok (get_last_id());
      send c2 @@ game @@ `Ready;
      ok   c2 @@ `Ok (get_last_id());
      (c1,c2)

let skip_turn c =
  send c @@ game @@ `Skip;
  ok   c @@ `Ok (get_last_id());
  send c @@ game @@ `Skip;
  ok   c @@ `Ok (get_last_id())

let buy c1 c2 card =
  (* デッキ:[銅*8; 領土*2]
     捨て札:[] *)
  send c1 @@ game `Skip;
  ok   c1 @@ `Ok (get_last_id());
  send c1 @@ game @@ `Select card;
  ok   c1 @@ `Ok (get_last_id());
  skip_turn c2;
  (* デッキ:[銅*2; 領土*3]
     捨て札:[銅*5; c*1] *)
  send c1 @@ game @@ `List `Mine;
  ok   c1 @@ `Cards (get_last_id(),[`Copper;`Copper;`Estate;`Estate; `Estate]);
  skip_turn c1; skip_turn c2;
  (* デッキ:[銅*2; 領土*3; 銅*5; c*1]
     捨て札:[] *)
  send c1 @@ game @@ `List `Mine;
  ok   c1 @@ `Cards (get_last_id(),[`Copper;`Copper;`Estate;`Estate; `Estate]);
  skip_turn c1; skip_turn c2;
  (* デッキ:[ 銅*5; c*1]
     捨て札:[ 銅*2; 領土*3] *)
  send c1 @@ game @@ `List `Mine;
  ok   c1 @@ `Cards (get_last_id(),[`Copper;`Copper;`Copper;`Copper;`Copper]);
  skip_turn c1; skip_turn c2;
  (* デッキ:[ c*1]
     捨て札:[ 銅*5; 銅*2; 領土*3] *)
  send c1 @@ game @@ `List `Mine;
  ok   c1 @@ `Cards (get_last_id(),[card; `Copper; `Copper; `Copper; `Copper])

let _ = begin "server.ml" >::: [
  "Listで作成したゲームした一覧が取得できる" >:: begin fun () ->
    let _ =
      M.run "some-server" 1729 in
    let c1 =
      S.connect "some-server" 1729 in
    let c2 =
      S.connect "some-server" 1729 in
      send c1 @@ `List (id());
      ok   c1 @@ `Games (get_last_id(),[]);
      send c2 @@ `Make (id(),"foo");
      ok   c2 @@ `Ok (get_last_id());
      send c1 @@ `List (id());
      ok   c1 @@ `Games (get_last_id(),["foo"]);
      send c2 @@ `List (id());
      ok   c2 @@ `Games (get_last_id(),["foo"]);
  end;
  "joinしてゲーム開始ができる" >:: begin fun () ->
    ignore @@ start ()
  end;
  "queryでガードを取得できる" >:: begin fun () ->
    let (c1,c2) =
      start () in
      send c1 @@ game @@ `List `Mine;
      ok c1 @@ `Cards (get_last_id(),[ `Copper; `Copper; `Copper; `Copper; `Copper]);
      send c2 @@ game @@ `List `Mine;
      ok c2 @@ `Cards (get_last_id(),[ `Copper; `Copper; `Copper; `Copper; `Copper])
  end;
  "自分のターンならskipできる" >:: begin fun () ->
    let (c1,c2) =
      start () in
      send c1 @@ game @@ `Skip;
      ok c1 @@ `Ok (get_last_id());
      send c2 @@ game @@ `Skip;
      ok c2 @@ `Error (get_last_id(),"not your turn");
  end;
  "ターン交代できる" >:: begin fun () ->
    let (c1,c2) =
      start () in
      skip_turn c1;
      skip_turn c2;
      skip_turn c1;
      skip_turn c2;
      skip_turn c1;
      skip_turn c2
  end;
  "買うとカードが増える" >:: begin fun () ->
    let (c1,c2) =
      start () in
      buy c1 c2 `Silver
  end;
  "カードが使える" >:: begin fun () ->
    let (c1,c2) =
      start () in
      buy c1 c2 `Moat;
      send c1 @@ game @@ `Select `Moat;
      ok   c1 @@ `Ok (get_last_id());

      (* draw 2 card *)
      send c1 @@ game @@ `List `Mine;
      ok   c1 @@ `Cards (get_last_id(),[`Copper;`Copper;`Copper;`Copper;`Copper;`Copper])
  end;
  "チャットできる" >:: begin fun () ->
    let (c1, c2) =
      start () in
      send c1 @@ `Game (game_name, `Message "hi");
      assert_recv (message @@ `Player ("alice","hi")) c1;
      assert_recv (message @@ `Player ("alice","hi")) c2;
      send c2 @@ `Game (game_name, `Message "hi");
      assert_recv (message @@ `Player ("bob","hi")) c1;
      assert_recv (message @@ `Player ("bob","hi")) c2;
  end;
  "ゲーム開始が通知される" >:: begin fun () ->
    let _ =
      history := [] in
    let (c1,c2) =
      start () in
      assert_recv (message @@ `GameStart) c1;
      assert_recv (message @@ `GameStart) c2
  end;
  "各ターンが通知される" >:: begin fun () ->
    let (c1,c2) =
      start () in
      assert_recv (message @@ `Turn "alice") c1;
      assert_recv (message @@ `Turn "alice") c2
  end;
  "フェーズごとに通知される" >:: begin fun () ->
    let _ =
      history := [] in
    let (c1,c2) =
      start () in
      send c1 @@ game @@ `Skip;
      ok   c1 @@ `Ok (get_last_id());
      send c1 @@ game @@ `Skip;
      ok   c1 @@ `Ok (get_last_id());

      send c2 @@ game @@ `Skip;
      ok   c2 @@ `Ok (get_last_id());
      send c2 @@ game @@ `Skip;
      ok   c2 @@ `Ok (get_last_id());

      assert_recv (message @@ `ActionPhase "alice")  c1;
      assert_recv (message @@ `ActionPhase "alice")  c2;
      assert_recv (message @@ `BuyPhase "alice")     c1;
      assert_recv (message @@ `BuyPhase "alice")     c2;
      assert_recv (message @@ `CleanupPhase "alice") c1;
      assert_recv (message @@ `CleanupPhase "alice") c2;

      assert_recv (message @@ `ActionPhase "alice")  c1;
      assert_recv (message @@ `ActionPhase "bob")  c2;
      assert_recv (message @@ `BuyPhase "bob")     c1;
      assert_recv (message @@ `BuyPhase "bob")     c2;
      assert_recv (message @@ `CleanupPhase "bob") c1;
      assert_recv (message @@ `CleanupPhase "bob") c2;
  end
] end +> run_test_xml_main
