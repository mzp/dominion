open Base
open Rule
open CardSource

class type t = object
  method request : string -> Game.card option Rule.t
  method me : string
  method others : string list
end

let card_action t = function
  | `Cellar ->
      (* - +１アクション
	 - 手札を好きな枚数捨て、同じ数だけ引く。 *)
      perform begin
	action t#me ((+)1);
	cs <-- many @@ hands t t#me;
	move (`Hands t#me) (`Discards t#me) cs;
	draw t#me @@ List.length cs
      end
  | `Market ->
      (* - +1 アクション
	 - +1 購入
	 - +1 コイン
	 - +1 ドロー *)
      perform begin
	action t#me ((+) 1);
	buy    t#me ((+) 1);
	coin   t#me ((+) 1);
	draw   t#me 1
      end
  | `Mine ->
      (* 手札のコイン1枚を処分し、そのコインのコスト+3以下のコイン
	 1枚を手札に加える。捨て山にではなく、手札に入る。*)
      perform begin
	c1 <-- in_treasures @@ hands t t#me;
	c2 <--
	  CardSource.filter (fun c _ -> Game.cost c <= Game.cost c1 + 3) @@
	  in_treasures @@
	  supply t t#me;
	move (`Hands t#me) `Trash        [ c1 ];
	move `Supply       (`Hands t#me) [ c2 ]
      end
  | `Remodel ->
      (* 手札のカードを1枚処分し、そのカードのコスト+2以下のコ
	 ストのカードを1枚取る。 *)
      perform begin
	c1 <-- hands t t#me;
	c2 <--
	  filter (fun c _ -> Game.cost c <= Game.cost c1 + 2) @@
	  supply t t#me;
	move (`Hands t#me) `Trash [ c1 ];
	move `Supply       (`Hands t#me) [ c2 ]
      end
  | `Smithy ->
      (* +3 ドロー *)
      draw t#me 3
  | `Village ->
      (* +1 ドロー
	 +2 アクション *)
      perform (action t#me @@ ((+) 2);
	       draw   t#me 1)
  | `Woodcutter ->
      (* - +2金
	 - +1購入 *)
      perform (buy  t#me @@ ((+) 1);
	       coin t#me @@ ((+) 2))
  | `Workshop ->
      (* コスト4以下のカードを1枚取る。 *)
      perform begin
	c <--
	  filter (fun c _ -> Game.cost c <= 4) @@
	  supply t t#me;
	move `Supply (`Hands t#me) [ c ]
      end
  | `Moat ->
      (* +2 ドロー *)
      draw t#me 2
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
	    filter (fun c _ -> c = `Moat) @@ hands t name
	end in
      let attack_to name =
	perform begin
	  p <-- player name;
	  let { Game.hands = cs; _ } = p in
	    guard (fun _ -> List.length cs > 3);
	    c <-- in_hands name @@
	      strict_source t name;
	    move (`Hands name) (`Discards name) [ c ]
	end in
	perform begin
	  coin t#me ((+) 2);
	  fold_m () t#others ~f:begin fun _ name ->
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
