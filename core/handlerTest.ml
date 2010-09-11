open Base
open OUnitUtil

(*
  ただしクライアントへのデータ送信はstubに置き換えて、
  テスト対象のモジュールを実体化する。
*)
module M = Handler.Make(struct
			  type t = int
			  let send _ _ = ()
			  let equal = (=)
			end)
open M

let rec run xs f g =
  let open Cc in
  let rec iter xs r =
    match xs, r with
	[], `End state ->
	  game state
      | (y::ys), `Cc (state, pred, cc) when pred y ->
	  iter ys (Cc.run (cc y state))
      | _::_, `Cc _ ->
	  failwith "unexpected request"
      | _::_, `End _ ->
	  failwith "too many request"
      | [], `Cc _ ->
	  failwith "require more request"
  in
  let state =
    make_dummy [1;2;3] g in
    iter xs @@ Cc.run @@ perform begin
      p <-- new_prompt ();
      pushP p @@ perform begin
	state <-- f p 42 state;
	return @@ `End state
      end
    end

let _ = begin "handler.ml" >::: [
  (let alice =
    Game.make_player "alice"
      ~hands:[ `Cellar; `Silver; `Silver ]
      ~decks:[ `Cellar; `Province; `Silver ] in
   let game =
     Game.make [ alice ] [ ] in
     "actionフェーズは" >::: [
       "スキップできる" >:: begin fun () ->
	 assert_equal game @@ run [`Skip] action game
       end;
       "指定したカードを使える" >:: begin fun () ->
	 let open Game in
	 let game' =
	   Game.update game ~f:(fun me -> { me with
					      action = 0;
					      hands = [`Cellar;`Province];
					      decks = [`Silver];
					      discards = [`Silver; `Silver]
					  }) in
	 let game' =
	   { game' with board = { game'.board with play_area  = [`Cellar] } } in
	 assert_equal game' @@ run [`Select `Cellar;
				    (* 捨てるカードの選択 *)
				    `Select `Silver;
				    `Select `Silver;
				    `Skip] action game
       end;
       "actionの回数だけカードを使える" >:: begin fun () ->
	 let open Game in
	 let game =
	   Game.update game
	     ~f:(fun me -> { me with action = 2 }) in
	 let game' =
	   Game.update game ~f:(fun me -> { me with
					      action = 0;
					      hands = [`Province];
					      discards = [`Silver; `Silver];
					      decks = [`Silver] }) in
	 let game' =
	   { game' with board = { game'.board with play_area  = [`Cellar; `Cellar] } } in

	 assert_equal ~printer:Std.dump game' @@ run [`Select `Cellar;
				    `Select `Silver;
				    `Skip;
				    `Select `Cellar;
				    `Select `Silver;
				    `Skip] action game
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
	 assert_equal game @@ run [`Skip] buy game
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
		   }) in
	   assert_equal ~printer:Std.dump game' @@ run [`Select `Cellar] buy game
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
		   }) in
	   assert_equal ~printer:Std.dump game' @@
	     run [`Select `Province; `Select `Cellar] buy game
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
		   }) in
	   assert_equal ~printer:Std.dump game' @@
	     run [`Select `Cellar; `Select `Cellar] buy game
       end
     ]);
  "cleanupフェーズは" >::: [
    "手札を捨て札に積む" >:: begin fun () ->
      let alice =
	Game.make_player "alice"
	  ~hands:[ `Gold; `Gold; ]
	  ~decks:[ `Gold; `Silver; `Copper; `Estate; `Duchy; `Gold ] in
      let { Game.hands; decks; discards; _ } =
	Game.me @@ run [] cleanup  @@ Game.make [ alice ] [] in
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
	Game.me @@ run [] cleanup  @@ Game.make [ alice ] [] in
	assert_equal ~msg:"hands" ~printer:(Std.dump $ List.map Game.to_string)[ `Gold; `Silver; `Silver; `Silver; `Silver] hands;
	assert_equal [ ] decks;
	assert_equal [ ] discards
    end;
  ]
] end +> run_test_xml_main

