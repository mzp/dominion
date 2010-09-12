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
  List.mem x !history

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

let ok t res =
  assert_equal ~printer:Std.dump res @@ wait_for t (get_last_id())

let concurrent xs =
  let e =
    ref None in
    List.iter Thread.join @@ List.map (fun f -> Thread.create (fun _ ->
								 try f () with e' -> e := Some e') ())
      xs;
    match !e with
	Some e -> raise e
      | None -> ()


let start _ =
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
      concurrent [
	(fun _ -> assert_equal (`Message (game_name,"alice","hi")) @@ recv c1);
	(fun _ -> assert_equal (`Message (game_name,"alice","hi")) @@ recv c2)
      ];
      send c2 @@ `Game (game_name, `Message "hi");
      concurrent [
	(fun _ -> assert_equal (`Message (game_name,"bob","hi")) @@ recv c1);
	(fun _ -> assert_equal (`Message (game_name,"bob","hi")) @@ recv c2)
      ];
  end
] end +> run_test_xml_main
