open Base
open ListUtil
open HandlerBase
open Rule

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type request = [
  | `Select of Game.card
  | `Skip
  ]
  type state = S.t HandlerBase.state

  let table : (S.t, request, Game.t) ContHandler.t =
    ContHandler.make ()

  type t = {
    me      : string;
    others  : string list;
    request : string -> request Rule.t
  }

  type client = {
    client : S.t;
    suspend: (S.t, request,  Game.t) ContHandler.suspend
  }
  let make_dummy _ = assert false
  let game _ = assert false

  (** 共通の情報の構築 *)
  let me state =
    Game.((current_player state).name)

  let players state =
    Game.(List.map (fun { name; _ } -> name) state.game.players)

  let others state =
    players state -- [ me state ]

  let skip =  function
      `Skip -> true
    | _     -> false
  let select = function
      `Select _ -> true
    | _         -> false
  let (<||>) f g x = f x || g x

  let request state suspend name : request Rule.t =
    let open Cc in
    let client =
      fst @@ List.find (fun (_,y)-> y = name) state.clients in
      Rule.lift (fun game ->
		   perform begin
		     request <-- suspend client (skip <||> select) game;
		     return (Left (request, game))
		   end)

  let make suspend state = {
    me = me state;
    others = others state;
    request = request state suspend
  }


  (** カードソース *)
  (* ユーザにカードを選んでもらう。(スキップ可) *)
  let card_source { request; _ } name : Game.card Rule.t  =
    perform begin
      r <-- request name;
      match r with
	  `Skip ->
	    error ""
	| `Select c ->
	    return c
    end

  (* ユーザにカードを選んでもらう。(スキップ不可) *)
  let rec strict_card_source t name : Game.card Rule.t =
    perform begin
      r <-- t.request name;
      match r with
	  `Skip ->
	    strict_card_source t name
	| `Select c ->
	    return c
    end

  (* カードソースにフィルタをかける *)
  let rec filter p cs =
    perform begin
      c <-- cs;
      b <-- p c;
      if b then
	return c
      else
	filter p cs
    end

  (* 単純なフィルタ *)
  let simple_filter p cs : Game.card Rule.t =
    filter (fun c -> Rule.game >>= (return $ p c)) cs

  let in_treasures =
    simple_filter (fun c _ -> Game.is_treasure c)

  let in_hands name =
    simple_filter (fun c g ->
		     match Game.find g name with
			 Some { Game.hands; _ } ->
			   List.mem c hands
		       | None ->
			   false)

  (* 手札のカードソース *)
  let hands t name =
    card_source t name
    +> in_hands name

  (* サプライのカードソース *)
  let supply t name =
    simple_filter (fun c g ->
		     List.mem c Game.(g.board.supply)) @@
      card_source t name

  (* アクションカード *)
  let card_action ({me; _} as t) = function
    | `Cellar ->
	(* - +１アクション
	   - 手札を好きな枚数捨て、同じ数だけ引く。 *)
	perform begin
	  action me ((+)1);
	  cs <-- many @@ hands t me;
	  move (`Hands me) (`Discards me) cs;
	  draw me @@ List.length cs
	end
    | `Market ->
	(* - +1 アクション
	   - +1 購入
	   - +1 コイン
	   - +1 ドロー *)
	perform begin
	  action me ((+) 1);
	  buy    me ((+) 1);
	  coin   me ((+) 1);
	  draw   me 1
	end
    | `Mine ->
	(* 手札のコイン1枚を処分し、そのコインのコスト+3以下のコイン
	   1枚を手札に加える。捨て山にではなく、手札に入る。*)
	perform begin
	  c1 <-- in_treasures @@ hands t me;
	  c2 <--
	    simple_filter (fun c _ -> Game.cost c <= Game.cost c1 + 3) @@
	    in_treasures @@
	    supply t me;
	  move (`Hands me) `Trash        [ c1 ];
	  move `Supply       (`Hands me) [ c2 ]
	end
    | `Remodel ->
	(* 手札のカードを1枚処分し、そのカードのコスト+2以下のコ
	   ストのカードを1枚取る。 *)
	perform begin
	  c1 <-- hands t me;
	  c2 <--
	    simple_filter (fun c _ -> Game.cost c <= Game.cost c1 + 2) @@
	    supply t me;
	  move (`Hands me) `Trash [ c1 ];
	  move `Supply       (`Hands me) [ c2 ]
	end
    | `Smithy ->
	(* +3 ドロー *)
	draw me 3
    | `Village ->
	(* +1 ドロー
	   +2 アクション *)
	perform (action me @@ ((+) 2);
		 draw   me 1)
    | `Woodcutter ->
	(* - +2金
	   - +1購入 *)
	perform (buy  me @@ ((+) 1);
		 coin me @@ ((+) 2))
    | `Workshop ->
	(* コスト4以下のカードを1枚取る。 *)
	perform begin
	  c <-- simple_filter (fun c _ -> Game.cost c <= 4) @@
	    supply t me;
	  move `Supply (`Hands me) [ c ]
	end
    | `Moat ->
	(* +2 ドロー *)
	draw me 2
    | `Militia ->
	(*- + 2 コイン
	  - 自分以外の全てのプレイヤーは手札が３枚になるまでカードを捨てる。 *)
	let player name =
	  lift (fun g -> match Game.find g name with
		    Some p ->
		      Cc.return @@ Left (p,g)
		  | None ->
		      Cc.return @@ Right "not found player") in
	let reaction name =
	  perform begin
	    p <-- player name;
	    let { Game.hands = cs; _ } = p in
	      guard (fun _ -> List.exists Game.is_reaction cs);
	      simple_filter (fun c _ -> c = `Moat) @@ hands t name
	  end in
	let attack_to name =
	  perform begin
	    p <-- player name;
	    let { Game.hands = cs; _ } = p in
	      guard (fun _ -> List.length cs > 3);
	      c <-- in_hands name @@
		strict_card_source t name;
	      move (`Hands name) (`Discards name) [ c ]
	    end in
	  perform begin
	    coin me ((+) 2);
	    fold_m () t.others ~f:begin fun _ name ->
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
      | _ ->
	  failwith "not action card"

  let many_ f =
    (many f) >> (return ())

  let action_phase ({ me; _ } as t) =
    many_ @@ perform begin
      guard @@ (fun g -> Game.((me g).action) <> 0);
      c <-- hands t me;
      move (`Hands me) `PlayArea [ c ];
      Rule.action me (fun n -> n - 1);
      card_action t c;
      return ()
    end

  let buy_phase ({ me; _ } as t) =
    let store : int Rule.t =
      perform begin
	g <-- Rule.game;
	let { Game.coin; hands; _ } =
	  Game.me g in
	return @@ coin + List.fold_left (+) 0 (List.map Game.coin hands)
      end
    in
      many_ @@ perform begin
	guard @@ (fun g -> Game.((me g).buy) <> 0);
	n <-- store;
	c <-- filter (fun c -> return (Game.cost c <= n)) @@
	  supply t me;
	move `Supply (`Discards me) [ c ];
	Rule.coin me (fun m -> m - Game.cost c);
	Rule.buy  me (fun m -> m - 1);
	return ()
      end

  (* cleanupフェーズ *)
  let cleanup_phase { me = me; _ }  =
    let open Rule in
      perform begin
	g <-- game;
	move (`Hands me) (`Discards me) Game.((me g).hands);
	draw me 5;
	action me @@ const 1;
	buy    me @@ const 1;
	coin   me @@ const 0
      end

  let turn state ({ me; _ } as t) =
    perform begin
      let _ = send_all state @@ `Turn me in
      (* action phase *)
      let _ = send_all state @@ `Phase (`Action, me) in
      action_phase t;
      (* buy phase *)
      let _ = send_all state @@ `Phase (`Buy, me) in
      buy_phase t;
      (* cleanup phase *)
      let _ = send_all state @@ `Phase (`Cleanup, me) in
      cleanup_phase t;
      (* turn swap *)
      g <-- Rule.game;
      set_game Game.({ g with me = (g.me + 1) mod List.length g.players})
    end

  let handle client request state =
    match  ContHandler.resume table client request with
	Left game ->
	  Left { state with game  }
      | Right msg ->
	  Right msg

  let invoke state =
    ignore @@ ContHandler.start table ~f:begin fun suspend ->
      let open Cc in
      let cc =
	Rule.run state.game ~f:(turn state @@ make suspend state) in
	perform begin
	  r <-- cc;
	  match r with
	      Left ((), game) ->
		ContHandler.end_ game
	    | Right msg ->
		failwith msg
	end
    end
end
