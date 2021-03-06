open Base
open OUnitUtil
open Turn

let run xs f game =
  let ys =
    ref (xs :> (string*Game.card option) list) in
  let players =
    Game.(List.map (fun x -> x.name) game.players) in
  let t = object
    method me     = List.hd players
    method others = List.tl players
    method observer = Observer.make ()
    method request name =
      Rule.lift (fun game ->
		   match !ys with
		       (name',x)::xs ->
			 assert_equal name' name;
			 ys := xs;
			 Cc.return (Left (x,game))
		     | [] ->
			 failwith "eof")
  end
  in
    match Cc.run @@ Rule.run ~f:(f t) game with
	Left ((),game) ->
	  game
      | Right msg ->
	  failwith msg

let assert_run' init f request expect =
  assert_equal ~printer:Std.dump expect @@
    run request f init

let assert_run init f request expect =
  assert_run' init f (List.map (fun x -> ("alice",x)) request) expect

let _ = begin "turn.ml" >::: [
  (let alice =
    Game.make_player "alice"
      ~hands:[ `Cellar; `Silver; `Silver ]
      ~decks:[ `Cellar; `Province; `Silver ] in
   let game =
     Game.make [ alice ] [ ] in
     "actionフェーズは" >::: [
       "スキップできる" >:: begin fun () ->
	 assert_run game action_phase [None]  game
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
	   assert_run game action_phase  [Some `Cellar;
					  (* 捨てるカードの選択 *)
					  Some `Silver;
					  Some `Silver;
					  None;
					  None] game'
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
	     Some `Cellar;
	     Some `Silver;
	     None;
	     Some `Cellar;
	     Some `Silver;
	     None;
	     None]
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
	 assert_run game buy_phase [None] game
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
	   assert_run game buy_phase [Some `Cellar] game'
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
	   assert_run game buy_phase [Some `Province; Some `Cellar; None] game'
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
	   assert_run game buy_phase [Some `Cellar; Some `Cellar] game'
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
] end +> run_test_xml_main


