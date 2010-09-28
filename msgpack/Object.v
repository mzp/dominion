(* -*- coding: utf-8 -*- *)
Require Import List.
Require Import BigEndian.

(** MsgPackで使うオブジェクトの定義 *)
Inductive object :=
| Bool (_ : bool)
| Nil
| PFixnum (_ : ascii8)
| NFixnum (_ : ascii8)
| Uint8  (_ : ascii8)
| Uint16 (_ : ascii16)
| Uint32 (_ : ascii32)
| Uint64 (_ : ascii64)
| Int8   (_ : ascii8)
| Int16  (_ : ascii16)
| Int32  (_ : ascii32)
| Int64  (_ : ascii64)
| Float  (_ : ascii32)
| Double (_ : ascii64)
| FixRaw (_ : list ascii8)
| Raw16  (_ : list ascii8)
| Raw32  (_ : list ascii8)
| FixArray ( _ : list object)
| Array16  ( _ : list object)
| Array32  ( _ : list object)
| FixMap   ( _ : list (object * object)%type)
| Map16    ( _ : list (object * object)%type)
| Map32    ( _ : list (object * object)%type).

(** 妥当なオブジェクトの定義 *)
Inductive Valid : object -> Prop :=
| VPFixNum : forall n,
  nat_of_ascii8 n < 128 -> Valid (PFixnum n)
| VNFixNum : forall n,
  (* 負の数を導入したくないので、補数表現を使う *)
  223 < nat_of_ascii8 n /\ nat_of_ascii8 n < 256 -> Valid (NFixnum n)
| VFixRaw : forall xs,
  length xs < pow 5 -> Valid (FixRaw xs)
| VRaw16 : forall xs,
  length xs < pow 16 -> Valid (Raw16 xs)
| VRaw32 : forall xs,
  length xs < pow 32 -> Valid (Raw32 xs)
| VFixArray : forall xs,
  length xs < pow 4 -> Valid (FixArray xs)
| VArray16  : forall xs,
  length xs < pow 16 -> Valid (Array16 xs)
| VArray32   : forall xs,
  length xs < pow 32 -> Valid (Array32 xs)
| VFixMap    : forall xs,
  length xs < pow 4 -> Valid (FixMap xs)
| VMap16     : forall xs,
  length xs < pow 16 -> Valid (Map16 xs)
| VMap32    : forall xs,
  length xs < pow 32 -> Valid (Map32 xs).

Lemma object_ind0 : forall P : object -> Prop,
  (forall b : bool, P (Bool b)) ->
  P Nil ->
  (forall a : ascii8, P (PFixnum a)) ->
  (forall a : ascii8, P (NFixnum a)) ->
  (forall a : ascii8, P (Uint8 a)) ->
  (forall a : ascii16, P (Uint16 a)) ->
  (forall a : ascii32, P (Uint32 a)) ->
  (forall a : ascii64, P (Uint64 a)) ->
  (forall a : ascii8, P (Int8 a)) ->
  (forall a : ascii16, P (Int16 a)) ->
  (forall a : ascii32, P (Int32 a)) ->
  (forall a : ascii64, P (Int64 a)) ->
  (forall a : ascii32, P (Float a)) ->
  (forall a : ascii64, P (Double a)) ->
  (forall l : list ascii8, P (FixRaw l)) ->
  (forall l : list ascii8, P (Raw16 l)) ->
  (forall l : list ascii8, P (Raw32 l)) ->
  P (FixArray nil) ->
  (forall (x : object) (xs : list object),
    P x -> P (FixArray xs) -> P (FixArray (x :: xs))) ->
  P (Array16 nil) ->
  (forall (x : object) (xs : list object),
    P x -> P (Array16 xs) -> P (Array16 (x :: xs))) ->
  P (Array32 nil) ->
  (forall (x : object) (xs : list object),
    P x -> P (Array32 xs) -> P (Array32 (x :: xs))) ->
  P (FixMap nil) ->
  (forall (x y : object) (xs : list (object * object)),
    P x -> P y -> P (FixMap xs) -> P (FixMap ((x, y) :: xs))) ->
  P (Map16 nil) ->
  (forall (x y : object) (xs : list (object * object)),
    P x -> P y -> P (Map16 xs) -> P (Map16 ((x, y) :: xs))) ->
  P (Map32 nil) ->
  (forall (x y : object) (xs : list (object * object)),
    P x -> P y -> P (Map32 xs) -> P (Map32 ((x, y) :: xs))) ->
  (forall o, P o).
Proof.
intros until o.
generalize o.
Check H23.
refine (fix obj_resolve o : P o :=
  match o with
    | FixArray xs =>
      (fix array_resolve (xs' : list object) : P (FixArray xs') :=
        match xs' with
          | nil => H16
          | x::xs => H17 x xs (obj_resolve x) _
        end) xs
    | Array16  xs =>
      (fix array_resolve (xs' : list object) : P (Array16 xs') :=
        match xs' with
          | nil =>H18
          | x::xs => H19 x xs (obj_resolve x) _
        end) xs
    | Array32  xs =>
      (fix array_resolve (xs' : list object) : P (Array32 xs') :=
        match xs' with
          | nil => H20
          | x::xs => H21 x xs (obj_resolve x) _
        end) xs
    | FixMap   xs =>
      (fix map_resolve (xs' : list (object * object)%type) : P (FixMap xs') :=
        match xs' with
          | nil => H22
          | (x,y)::xs =>
            H23 x y xs (obj_resolve x) (obj_resolve y) _
        end) xs
    | Map16    xs =>
      (fix map_resolve (xs' : list (object * object)%type) : P (Map16 xs') :=
        match xs' with
          | nil => H24
          | (x,y)::xs =>
            H25 x y xs (obj_resolve x) (obj_resolve y) _
        end) xs
    | Map32    xs =>
      (fix map_resolve (xs' : list (object * object)%type) : P (Map32 xs') :=
        match xs' with
          | nil => H26
          | (x,y)::xs =>
            H27 x y xs (obj_resolve x) (obj_resolve y) _
        end) xs
    | _ => _
  end); auto.
Qed.
