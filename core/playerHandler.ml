open Base
open Cc
open ListUtil
open Game
open HandlerBase

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type request = [
  | `Select of Game.card
  | `Skip
  ]
  type state = S.t HandlerBase.state

  let table : (S.t, request, state) ContHandler.t =
    ContHandler.make ()

  let handle =
    ContHandler.resume table

  type client = {
    client : S.t;
    suspend: (S.t, request, state) ContHandler.suspend
  }

  (* ユーザに情報を送る *)
  let send { client; _ } e =
    S.send client e

  let notify { client; _ } s =
    S.send client @@ `Notify s

  let ok { client; _ } =
    S.send client `Ok

  let error { client; _ } s =
    S.send client @@  `Error s

  let update_game ~f state =
    { state with game = f state.game }


  (* skipリクエストならtrueを返す *)
  let skip =  function
      `Skip -> true
    | _     -> false

  (* selectリクエストならtrueを返す *)
  let select = function
      `Select _ -> true
    | _         -> false

  (* 述語の合成 *)
  let bin_op op f g x =
    op (f x) (g x)

  let (<||>) = bin_op (||)


  let me state =
    (current_player state).name

  let players state =
    List.map (fun { name; _ } -> name) state.game.players

  let others state =
    players state -- [ me state ]

  let rec fold_m ~f a =
    let open Rule in
    function
      | [] ->
	  return a
      | x::xs ->
	  perform (y <-- f a x;
		   fold_m ~f y xs)

  let request name ~p { suspend; _ } state : request Rule.t =
    let open Cc in
    let client =
      fst @@ List.find (fun (_,y)-> y = name) state.clients in
      Rule.lift (fun game ->
		   perform begin
		     (request, _) <-- suspend client p state;
		     return (Left (request, game))
		   end)

  let wrap state cc =
    let open Cc in
      perform begin
	r <-- cc;
	match r with
	    Left (_, game) ->
	      Cc.return { state with game }
	  | Right _ ->
	      failwith ""
      end

  let rec card_source name client state =
    let open Rule in
      perform begin
	r <-- request name ~p:(skip <||> select) client state;
	match r with
	    `Skip ->
	      error ""
	  | `Select c ->
	      return c
      end

  let rec strict_card_source name client state =
    let open Rule in
      perform begin
	r <-- request name ~p:(skip <||> select) client state;
	match r with
	    `Skip ->
	      strict_card_source name client state
	  | `Select c ->
	      return c
      end

  let rec filter p cs =
    let open Rule in
      perform begin
	c <-- cs;
	b <-- p c;
	if b then
	  return c
	else
	  filter p cs
      end

  let simple_filter p cs =
    let open Rule in
      filter (fun c -> game >>= (fun g -> return @@ p c g)) cs

  let treasures =
    simple_filter (fun c _ -> is_treasure c)

  let hands_only name =
    let open Rule in
      simple_filter (fun c g ->
		       match Game.find g name with
			   Some { hands; _ } ->
			     List.mem c hands
			 | None ->
			     false)

  let hands name client state =
    let open Rule in
      card_source name client state
      +> hands_only name

  let supply name client state =
    let open Rule in
      simple_filter (fun c g ->
		       List.mem c g.board.supply) @@
	card_source name client state

  let guard f =
    let open Rule in
      perform begin
	g <-- game;
	if f g then
	  return ()
	else
	  error ""
      end

  (* Actionカードの定義 *)
  let card_action kind client state=
    let me =
      me state in
    let name =
      me in
    match kind with
      | `Cellar ->
	  (* - +１アクション
	     - 手札を好きな枚数捨て、同じ数だけ引く。 *)
	  let open Rule in
	  wrap state @@ Rule.run state.game ~f:begin
	    perform begin
	      action name ((+)1);
	      cs <-- many @@ hands name client state;
	      move (`Hands name) (`Discards name) cs;
	      draw name @@ List.length cs
	    end
	  end
      | `Market ->
	  (* - +1 アクション
	     - +1 購入
	     - +1 コイン
	     - +1 ドロー *)
	  let open Rule in
	  wrap state @@ Rule.run state.game ~f:begin
	    perform begin
	      action name ((+) 1);
	      buy    name ((+) 1);
	      coin   name ((+) 1);
	      draw   name 1
	    end
	  end
      | `Mine ->
	  (* 手札のコイン1枚を処分し、そのコインのコスト+3以下のコイン
	     1枚を手札に加える。捨て山にではなく、手札に入る。*)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform begin
		c1 <-- treasures @@ hands name client state;
		c2 <--
		  simple_filter (fun c _ -> Game.cost c <= Game.cost c1 + 3) @@
		  treasures @@
		  supply name client state;
		move (`Hands name) `Trash        [ c1 ];
		move `Supply       (`Hands name) [ c2 ]
	      end
	    end
      | `Remodel ->
	  (* 手札のカードを1枚処分し、そのカードのコスト+2以下のコ
	     ストのカードを1枚取る。 *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform begin
		c1 <-- hands name client state;
		c2 <--
		  simple_filter (fun c _ -> Game.cost c <= Game.cost c1 + 2) @@
		  supply name client state;
		move (`Hands name) `Trash [ c1 ];
		move `Supply       (`Hands name) [ c2 ]
	      end
	    end
      | `Smithy ->
	  (* +3 ドロー *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:(draw name 3)
      | `Village ->
	  (* +1 ドロー
	     +2 アクション *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform (action name @@ ((+) 2);
		       draw   name 1)
	    end
      | `Woodcutter ->
	  (* - +2金
	     - +1購入 *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform (buy  name @@ ((+) 1);
		       coin name @@ ((+) 2))
	    end
      | `Workshop ->
	  (* コスト4以下のカードを1枚取る。 *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform begin
		c <-- simple_filter (fun c _ -> Game.cost c <= 4) @@
		  supply name client state;
		move `Supply (`Hands name) [ c ]
	      end
	    end
      | `Moat ->
	  (* +2 ドロー *)
	  let open Rule in
	    wrap state @@ Rule.run state.game ~f:(draw name 2)
      | `Militia ->
	  (*- + 2 コイン
	    - 自分以外の全てのプレイヤーは手札が３枚になるまでカードを捨てる。 *)
	  let open Rule in
	  let player name =
	    lift (fun g -> match Game.find g name with
		      Some p ->
			Cc.return @@ Left (p,g)
		    | None ->
			Cc.return @@ Right "not found player") in
	  let reaction name =
	    perform begin
	      p <-- player name;
	      let { hands = cs; _ } = p in
	      guard (fun _ -> List.exists Game.is_reaction cs);
	      simple_filter (fun c _ -> c = `Moat) @@ hands name client state
	    end in
	  let attack_to name =
	    perform begin
	      p <-- player name;
	      let { hands = cs; _ } = p in
	      guard (fun _ -> List.length cs > 3);
	      c <-- hands_only name @@
	      strict_card_source name client state;
	      move (`Hands name) (`Discards name) [ c ]
	    end in
	    wrap state @@ Rule.run state.game ~f:begin
	      perform begin
		coin name ((+) 2);
		fold_m () (others state) ~f:begin fun _ name ->
		  perform begin
		    c <-- option (reaction name);
		    match c with
			Some `Moat ->
			  return ()
		      | Some _ | None ->
			  (many @@ attack_to name) >> (return ())
		  end
		end
	      end
	    end
      | _ ->
	  failwith "not action card"

  let effect c client state : 'a Rule.t =
    Rule.lift (fun game ->
		 let open Cc in
		   perform (state <-- card_action c client { state with game };
			    return @@ Left ((),state.game)))

  (* actionフェーズ *)
  let action_phase client state =
    let open Rule in
    let name =
      me state in
      wrap state @@ Rule.run state.game ~f:begin
	Rule.many @@ perform begin
	  guard @@ (fun g -> (Game.me g).action <> 0);
	  c <-- hands name client state;
	  move (`Hands name) `PlayArea [ c ];
	  Rule.action name (fun n -> n - 1);
	  effect c client state;
	  return ()
	end
      end

  (* buyフェーズ *)
  let buy_phase client state =
    let open Rule in
    let name =
      me state in
    let store =
      perform (g <-- game;
	       let { coin; hands; _ } =
		 Game.me g in
		 return @@ coin + List.fold_left (+) 0 (List.map Game.coin hands)) in
      wrap state @@ Rule.run state.game ~f:begin
	many @@ perform begin
	  guard @@ (fun g -> (Game.me g).buy <> 0);
	  n <-- store;
	  c <-- filter (fun c -> return (Game.cost c <= n)) @@
	    supply name client state;
	  move `Supply (`Discards name) [ c ];
	  Rule.coin name (fun m -> m - Game.cost c);
	  Rule.buy  name (fun m -> m - 1);
	  return ()
	end
      end

  (* cleanupフェーズ *)
  let cleanup_phase _ state =
    let open Rule in
    let me =
      me state in
      wrap state @@ Rule.run state.game ~f:perform begin
	move (`Hands me) (`Discards me) (current_player state).hands;
	draw me 5;
	action me @@ const 1;
	buy    me @@ const 1;
	coin   me @@ const 0
      end

  let turn client state =
    let log name cs =
      Logger.debug "%s: %s" name (Std.dump (List.map Game.to_string cs)) () in
    let me =
      current_player state in
    let name =
      me.Game.name in
      perform begin
	let _ =
	  Logger.debug "player : %s" name ();
	  log "decks" me.Game.decks;
	  log "hands" me.Game.hands;
	  log "discards" me.Game.discards in
	let _ = send_all state @@ `Turn name in
	  (* action phase *)
	let _ = send_all state @@ `Phase (`Action, name) in
	  state <-- action_phase client state;
	  (* buy phase *)
	  let _ = send_all state @@ `Phase (`Buy, name) in
	    state <-- buy_phase client state;
	    (* cleanup phase *)
	    let _ = send_all state @@ `Phase (`Cleanup, name) in
	      state <-- cleanup_phase client state;
	      let state = update_game state
		~f:(fun g ->
		      { g with me = (g.me + 1) mod List.length g.players}) in
		ContHandler.end_ state
      end

  let invoke state =
    let client =
      current_client state in
      ignore @@ ContHandler.start table
	~f:(fun suspend ->  turn { suspend; client })
	state

  (* for test *)
  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients= HList.zip
	xs
	(List.map (fun {name; _ } -> name) g.players);
      ready=xs;
      playing =true }
end
