open Base
open Rule

class type t = object
  method request : string -> Game.card option Rule.t
end

(* ユーザにカードを選んでもらう。(スキップ可) *)
let source t name : Game.card Rule.t  =
  perform begin
    r <-- t#request name;
    match r with
      | Some c ->
	  return c
      | None ->
	  error ""
  end

(* ユーザにカードを選んでもらう。(スキップ不可) *)
let rec strict_source t name : Game.card Rule.t =
  perform begin
    r <-- t#request name;
    match r with
	None ->
	  strict_source t name
      | Some c ->
	  return c
  end

(* 単純なフィルタ *)
let filter p cs : Game.card Rule.t =
  Rule.filter (fun c -> Rule.game >>= (return $ p c)) cs

let in_treasures =
  filter (fun c _ -> Game.is_treasure c)

let in_hands name =
  filter (fun c g ->
		   match Game.find g name with
		       Some { Game.hands; _ } ->
			 List.mem c hands
		     | None ->
			 false)

(* 手札のカードソース *)
let hands t name =
  source t name
  +> in_hands name

(* サプライのカードソース *)
let supply t name =
  filter (fun c g ->
		   List.mem c Game.(g.board.supply)) @@
    source t name
