(* -*- coding:utf-8 -*- *)
open Base
open Rule
open CardSource

class type t = object
  method request : string -> Game.card option Rule.t
  method me : string
  method others : string list
  method observer : Protocol.game_response Observer.t
end

let fire t e =
  lift (fun game ->
	  Observer.__fire t#observer e;
	  Cc.return (Left ((),game)))

let action_phase t =
  many_ @@ perform begin
    guard @@ (fun g -> Game.((me g).action) <> 0);
    c <-- hands t t#me;
    move (`Hands t#me) `PlayArea [ c ];
    Rule.action t#me (fun n -> n - 1);
    ActionCard.effect t c;
    return ()
  end

let buy_phase t =
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
      c <-- Rule.filter (fun c -> return (Game.cost c <= n)) @@
	supply t t#me;
      move `Supply (`Discards t#me) [ c ];
      Rule.coin t#me (fun m -> m - Game.cost c);
      Rule.buy  t#me (fun m -> m - 1);
      return ()
    end

(* cleanupフェーズ *)
let cleanup_phase t  =
  perform begin
    g <-- game;
    move (`Hands t#me) (`Discards t#me) Game.((me g).hands);
    draw   t#me 5;
    action t#me @@ const 1;
    buy    t#me @@ const 1;
    coin   t#me @@ const 0
  end

let turn t =
  perform begin
    fire t (`Turn t#me);
    fire t (`ActionPhase t#me);
    action_phase t;
    fire t (`BuyPhase t#me);
    buy_phase t;
    fire t (`CleanupPhase t#me);
    cleanup_phase t;
    g <-- Rule.game;
    set_game Game.({ g with me = (g.me + 1) mod List.length g.players})
  end
