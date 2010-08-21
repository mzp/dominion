let bind m f =
  match m with
      None -> None
    | Some v -> f v

let (>>=) = bind

let return x =
  Some x
