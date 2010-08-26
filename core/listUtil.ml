open Base

let find p xs =
  (option (List.find p)) xs

let add x xs =
  if List.mem x xs then
    xs
  else
    x :: xs

let (--) xs ys =
  List.fold_left (fun xs' y -> ExtList.List.remove xs' y) xs ys

let shuffle xs =
  Random.self_init ();
  List.map (fun x -> (Random.int (List.length xs), x)) xs
  +> List.sort (fun (x,_) (y,_) -> compare x y)
  +> List.map snd
