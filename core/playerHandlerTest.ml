open Base
open OUnit

(*
  ただしクライアントへのデータ送信はstubに置き換えて、
  テスト対象のモジュールを実体化する。
*)
module M = PlayerHandler.Make(struct
				type t = int
				let send _ _ = ()
				let equal = (=)
			      end)
open M


let run xs f g =
  let open Cc in
  let table =
    ContHandler.make () in
  let rec iter state = function
      [] ->
	game state
    | (client,x)::xs ->
	match ContHandler.resume table client x state with
	    Left state ->
	      iter state xs
	  | Right msg ->
	      failwith msg
  in
  let state =
    make_dummy [1;2;3] g
  in
    match ContHandler.start table state
      ~f:(fun suspend state ->
	    perform begin
	      state <-- f {suspend; client=1} state;
	      ContHandler.end_ state
	    end) with
	Left state ->
	  iter state xs
      | Right x ->
	  failwith x

let assert_run' init f request expect =
  assert_equal ~printer:Std.dump expect @@
    run request f init

let assert_run init f request expect =
  assert_run' init f (List.map (fun x -> (1,x)) request) expect

open Game
let supply xs g =
  Game.update_board g ~f:(fun b -> { b with supply = xs })
let trash xs g =
  Game.update_board g ~f:(fun b -> { b with trash = xs })


let card_action_test  =
    let alice hands decks =
      Game.make_player "alice" ~hands ~decks in
    let make me =
      Game.make [ me ] [] in
      "カードの効果" >::: [
	"cellar" >:: begin fun () ->
	  let me =
	    alice [ `Silver; `Silver ] [ `Cellar; `Province; `Silver ] in
	    assert_run (make me) (card_action `Cellar)
	      [`Select `Silver; `Select `Silver; `Skip] @@
	      make { me with
		       action = 2;
		       hands = [`Cellar;`Province];
		       decks = [`Silver];
		       discards = [`Silver; `Silver] }
	end;
	"market" >:: begin fun () ->
	  let me =
	    alice [ ] [ `Gold ] in
	    assert_run (make me) (card_action `Market)
	      [] @@
	      make { me with
		       action = 2;
		       buy    = 2;
		       coin   = 1;
		       hands = [`Gold];
		       decks = [];
		       discards = [] }
	end;
	"mine" >:: begin fun () ->
	  let me =
	    alice [ `Silver; `Cellar ] [] in
	  let game =
	    update_board (make me)
	      ~f:(fun b -> { b with supply = [ `Gold; `Cellar ] }) in
	  let game' =
	    update_board (make { me with hands=[`Gold; `Cellar]})
	      ~f:(fun b -> { b with supply = [`Cellar]; trash = [ `Silver ] }) in
	    assert_run game (card_action `Mine) [`Select `Silver; `Select `Gold] game';
	    assert_run game (card_action `Mine) [`Select `Cellar; `Select `Silver; `Select `Gold] game';
	    assert_run game (card_action `Mine) [`Select `Mine; `Select `Silver; `Select `Gold] game';
	    assert_run game (card_action `Mine) [`Select `Silver; `Select `Cellar; `Select `Gold] game';
	end;
	"remodel" >:: begin fun () ->
	  let me =
	    alice [ `Copper ] [] in
	  let game =
	    update_board (make me)
	      ~f:(fun b -> { b with supply = [ `Gold; `Cellar ] }) in
	  let game' =
	    update_board (make { me with hands=[ `Cellar ]})
	      ~f:(fun b -> { b with supply = [`Gold ]; trash = [ `Copper ] }) in
	    assert_run game (card_action `Remodel) [`Select `Copper; `Select `Cellar] game';
	    assert_run game (card_action `Remodel) [`Select `Copper; `Select `Gold; `Select `Cellar] game'
	end;
	"smithy" >:: begin fun () ->
	  let me =
	    alice [ ] [`Silver;`Silver;`Silver] in
	  let game =
	    make me in
	  let game' =
	    make { me with hands=[ `Silver;`Silver;`Silver ]; decks=[]} in
	    assert_run game (card_action `Smithy) [] game'
	end;
	"village" >:: begin fun () ->
	  let me =
	    alice [ ] [`Silver] in
	  let game =
	    make me in
	  let game' =
	    make { me with hands=[ `Silver]; decks=[]; action = 3 } in
	    assert_run game (card_action `Village) [] game'
	end;
	"woodcutter" >:: begin fun () ->
	  let me =
	    alice [] [] in
	  let game =
	    make me in
	  let game' =
	    make { me with coin = 2; buy = 2 } in
	    assert_run game (card_action `Woodcutter) [] game'
	end;
	"workshop" >:: begin fun () ->
	  let me =
	    alice [] [] in
	  let game =
	    update_board (make me)
	      ~f:(fun b -> { b with supply = [ `Gold; `Cellar ] }) in
	  let game' =
	    update_board (make { me with hands=[`Cellar]})
	      ~f:(fun b -> { b with supply = [`Gold] }) in
	    assert_run game (card_action `Workshop) [`Select `Cellar] game';
	    assert_run game (card_action `Workshop) [`Select `Gold; `Select `Cellar] game'
	end;
	"moat" >:: begin fun () ->
	  let game =
	    make @@ alice [ ] [`Silver; `Silver] in
	  let game' =
	    make @@ alice [ `Silver; `Silver ] [] in
	    assert_run game (card_action `Moat) [] game'
	end;
	"militia" >:: begin fun () ->
	  let player name hands =
	    Game.make_player name ~hands ~decks:[] in
	  let p1 =
	    player "A" [] in
	  let p2 =
	    player "B" [`Gold; `Gold; `Gold; `Gold; `Gold] in
	  let p3 =
	    player "C" [`Moat; `Gold; `Gold; `Gold; `Gold] in
	  let game =
	    Game.make [p1;p2;p3] [] in
	  let game' =
	    Game.make [{ p1 with coin = 2};
		       { p2 with hands = [`Gold;`Gold;`Gold]; discards=[`Gold;`Gold]};
		       p3] [] in
	    assert_run' game (card_action `Militia) [
	      (* Bが捨てるカードを選ぶ *)
	      (2,`Select `Gold);
	      (2,`Select `Gold);
	      (* Aがreactionを出す *)
	      (3,`Select `Moat)
	    ] game'
	end
      ]

let _ = begin "playerHandler.ml" >::: [
  card_action_test;
  (let alice =
    Game.make_player "alice"
      ~hands:[ `Cellar; `Silver; `Silver ]
      ~decks:[ `Cellar; `Province; `Silver ] in
   let game =
     Game.make [ alice ] [ ] in
     "actionフェーズは" >::: [
       "スキップできる" >:: begin fun () ->
	 assert_run game action_phase [`Skip]  game
       end;
       "指定したカードを使える" >:: begin fun () ->
	 let open Game in
	 let game' =
	   Game.update game ~f:(fun me -> { me with
					      action = 1;
					      hands = [`Cellar;`Province];
					      decks = [`Silver];
					      discards = [`Silver; `Silver]
					  }) in
	 let game' =
	   { game' with board = { game'.board with play_area  = [`Cellar] } } in
	   assert_run game action_phase  [`Select `Cellar;
					  (* 捨てるカードの選択 *)
					  `Select `Silver;
					  `Select `Silver;
					  `Skip;
					  `Skip] game'
       end;
       "actionの回数だけカードを使える" >:: begin fun () ->
	 let open Game in
	 let game =
	   Game.update game
	     ~f:(fun me -> { me with action = 2 }) in
	 let game' =
	   Game.update game ~f:(fun me -> { me with
					      action = 2;
					      hands = [`Province];
					      discards = [`Silver; `Silver];
					      decks = [`Silver] }) in
	 let game' =
	   { game' with board = { game'.board with play_area  = [`Cellar; `Cellar] } } in

	   assert_run game action_phase [
	     `Select `Cellar;
	     `Select `Silver;
	     `Skip;
	     `Select `Cellar;
	     `Select `Silver;
	     `Skip;
	     `Skip]
	     game'
       end
     ]);
  (let alice =
    Game.make_player "alice"
      ~hands:[ `Gold ]
      ~decks:[] in
   let game =
     Game.make [ alice ] [ `Cellar; `Province; `Cellar ] in
     "buyフェーズは" >::: [
       "スキップできる" >:: begin fun () ->
	 assert_run game buy_phase [`Skip] game
       end;
       "指定したカードを買える" >:: begin fun () ->
	 let open Game in
	 let game' =
	   Game.update game
	     ~f:(fun me ->
		   { me with
		       discards = `Cellar::me.discards;
		       buy = 0;
		       coin = -2;
		   })
	   +> (fun s -> { s with board = { s.board with supply = [`Province; `Cellar]}}) in
	   assert_run game buy_phase [`Select `Cellar] game'
       end;
       "高すぎるカードは買えない" >:: begin fun () ->
	 let open Game in
	 let game' =
	   Game.update game
	     ~f:(fun me ->
		   { me with
		       discards = `Cellar::me.discards;
		       buy = 0;
		       coin = -2;
		   })
	   +> (fun s -> { s with board = { s.board with supply = [`Province; `Cellar]}}) in
	   assert_run game buy_phase [`Select `Province; `Select `Cellar] game'
       end;
       "buyの回数だけカードを買える" >:: begin fun () ->
	 let open Game in
	 let game =
	   Game.update game ~f:(fun me ->
				  { me with buy = 2 }) in
	 let game' =
	   Game.update game
	     ~f:(fun me ->
		   { me with
		       discards = `Cellar::`Cellar::me.discards;
		       buy = 0;
		       coin = -4;
		   })
	   +> (fun s -> { s with board = { s.board with supply = [`Province ]}}) in
	   assert_run game buy_phase [`Select `Cellar; `Select `Cellar] game'
       end
     ]);
  "cleanupフェーズは" >::: [
    "手札を捨て札に積む" >:: begin fun () ->
      let alice =
	Game.make_player "alice"
	  ~hands:[ `Gold; `Gold; ]
	  ~decks:[ `Gold; `Silver; `Copper; `Estate; `Duchy; `Gold ] in
      let { Game.hands; decks; discards; _ } =
	Game.me @@ run [] cleanup_phase  @@ Game.make [ alice ] [] in
	assert_equal [ `Gold; `Silver; `Copper; `Estate; `Duchy ] hands;
	assert_equal [ `Gold ] decks;
	assert_equal [ `Gold; `Gold ] discards
    end;
    "デッキの枚数がすくないときは捨て札をデッキに戻す" >:: begin fun () ->
      let alice =
	Game.make_player "alice"
	  ~hands:[ ]
	  ~decks:[ `Gold ] in
      let alice =
	{ alice with
	    Game.discards = [`Silver; `Silver; `Silver; `Silver ] } in
      let { Game.hands; decks; discards; _ } =
	Game.me @@ run [] cleanup_phase  @@ Game.make [ alice ] [] in
	assert_equal ~msg:"hands" ~printer:(Std.dump $ List.map Game.to_string)[ `Gold; `Silver; `Silver; `Silver; `Silver] hands;
	assert_equal [ ] decks;
	assert_equal [ ] discards
    end;
  ]
] end +> run_test_tt_main

