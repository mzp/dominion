open Base

module Make(S : Protocol.Rpc)(B : HandlerBase.S with type t = S.t)  = struct
  open B
  open ListUtil
  type request = [
  | `Ready ]

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
    let state' =
      if List.exists (fun (c,_) -> c = client) clients then
	begin
	  S.send client @@ `Ok;
	  { state with ready = add client ready }
	end else begin
	  S.send client @@ `Error "error";
	  state
	end in
      if List.length state'.ready = List.length state'.clients then begin
	send_all state' `GameStart;
	{ state' with
	    playing = true;
	    game = game state' }
      end else
	state'
end
