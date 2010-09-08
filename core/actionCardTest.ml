open Base
open OUnit
open ActionCard
open Game

let run xs f game =
  let ys =
    ref (xs :> (string*Game.card option) list) in
  let players =
    Game.(List.map (fun x -> x.name) game.players) in
  let t = object
    method me = List.hd players
    method others = List.tl players
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


let alice hands decks =
  Game.make_player "alice" ~hands ~decks

let make me =
  Game.make [ me ] []

let effect =
  flip ActionCard.effect

let _ = begin "actionCard.ml" >::: [
  "cellar" >:: begin fun () ->
    let me =
      alice [ `Silver; `Silver ] [ `Cellar; `Province; `Silver ] in
      assert_run (make me) (effect `Cellar)
	[Some `Silver; Some `Silver; None] @@
	make { me with
		 action = 2;
		 hands = [`Cellar;`Province];
		 decks = [`Silver];
		 discards = [`Silver; `Silver] }
  end;
  "market" >:: begin fun () ->
    let me =
      alice [ ] [ `Gold ] in
      assert_run (make me) (effect `Market)
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
      assert_run game (effect `Mine) [Some `Silver; Some `Gold] game';
      assert_run game (effect `Mine) [Some `Cellar; Some `Silver; Some `Gold] game';
      assert_run game (effect `Mine) [Some `Mine; Some `Silver; Some `Gold] game';
      assert_run game (effect `Mine) [Some `Silver; Some `Cellar; Some `Gold] game';
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
      assert_run game (effect `Remodel) [Some `Copper; Some `Cellar] game';
      assert_run game (effect `Remodel) [Some `Copper; Some `Gold; Some `Cellar] game'
  end;
  "smithy" >:: begin fun () ->
    let me =
      alice [ ] [`Silver;`Silver;`Silver] in
    let game =
      make me in
    let game' =
      make { me with hands=[ `Silver;`Silver;`Silver ]; decks=[]} in
      assert_run game (effect `Smithy) [] game'
  end;
  "village" >:: begin fun () ->
    let me =
      alice [ ] [`Silver] in
    let game =
      make me in
    let game' =
      make { me with hands=[ `Silver]; decks=[]; action = 3 } in
      assert_run game (effect `Village) [] game'
  end;
  "woodcutter" >:: begin fun () ->
    let me =
      alice [] [] in
    let game =
      make me in
    let game' =
      make { me with coin = 2; buy = 2 } in
      assert_run game (effect `Woodcutter) [] game'
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
      assert_run game (effect `Workshop) [Some `Cellar] game';
      assert_run game (effect `Workshop) [Some `Gold; Some `Cellar] game'
  end;
  "moat" >:: begin fun () ->
    let game =
      make @@ alice [ ] [`Silver; `Silver] in
    let game' =
      make @@ alice [ `Silver; `Silver ] [] in
      assert_run game (effect `Moat) [] game'
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
      assert_run' game (effect `Militia) [
	(* Bが捨てるカードを選ぶ *)
	("B",Some `Gold);
	("B",Some `Gold);
	(* Aがreactionを出す *)
	("C",Some `Moat)
      ] game'
  end


] end +> run_test_tt_main

