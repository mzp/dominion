open Base
type 'a listener = 'a -> unit
type 'a t = 'a listener list ref


let listen o f =
  o := f :: !o

let make () =
  ref []

let __fire o x =
  List.iter (fun f -> f x) !o

let clear o =
  o := []

let map f o =
  let o' =
    make () in
    listen o (fun x -> __fire o' @@ f x);
    o'

let filter f o =
  let o' =
    make () in
    listen o (fun x ->
		match f x with
		    None -> ()
		  | Some y -> __fire o' y);
    o'

let merge o1 o2 =
  let o' =
    make () in
    listen o1 (fun x -> __fire o' (Left x));
    listen o2 (fun x -> __fire o' (Right x));
    o'

