open Base
open HandlerBase
open ListUtil

module Make(S : Protocol.Rpc)  = struct
  module B = HandlerBase.Make(S)
  open B

  type state = S.t HandlerBase.state
  type request = [ `Ready ]

  let game { clients; _ } =
    let players =
      ListLabels.map clients ~f:begin fun (_,name)->
	let init =
	  ListUtil.shuffle @@ List.concat [
	    HList.replicate 7 `Copper;
	    HList.replicate 3 `Estate;
	  ] in
	let (hands, decks) =
	  HList.splitAt 5 init in
	  Game.make_player name ~hands ~decks
      end in
    let kindgdoms =
      HList.concat_map (HList.replicate 10) [
	`Cellar;
	`Market;
	`Mine;
	`Remodel;
	`Smithy;
	`Village;
	`Woodcutter;
	`Workshop;
	`Militia;
	`Moat;
      ] in
    let treasures =
      HList.concat_map (HList.replicate 30) [
	`Gold;
	`Silver;
	`Copper
      ] in
    let victories =
      HList.concat_map (HList.replicate (if List.length players <= 2 then 8 else 12)) [
	`Estate;
	`Duchy;
	`Province
      ] in
    let cards =
      List.concat [ kindgdoms; treasures; victories ] in
      Game.make players cards

  let handle client _ ({ clients; ready; _ } as state) =
    if List.exists (fun (c,_) -> c = client) clients then
      let state' =
	{ state with ready = add client ready } in
	if List.length state'.ready = List.length state'.clients then begin
	  send_all state' `GameStart;
	  Left { state' with
		   playing = true;
		   game = game state' }
	end else
	  Left state'
    else
      Right  "error"
end
