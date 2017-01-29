open Core.Std
open Utils

type t = int option list list

let grid_size = 4

let empty =
  [ [ None; None; None; None ];
    [ None; None; None; None ];
    [ None; None; None; None ];
    [ None; None; None; None ]; ]

let sample_new () =
  if Random.int 4 < 3 then Some 2 else Some 4

let add_random grid =
  let nb_none = List.fold ~init:0 ~f:(+) (
      List.map ~f:(List.count ~f:Option.is_none) grid
  ) in
  (* FIXME can this be used for end of game detection? *)
  let () = assert (nb_none > 0) in
  let new_id = Random.int nb_none in
  List.fold_right grid ~init:(0, []) ~f:(fun row (count, res) ->
    let (count, row') =
      List.fold_right row ~init:(count, []) ~f:( fun elem (c, r) ->
        match elem with
        | None -> if c = new_id then (c + 1, sample_new () :: r)
                                else (c + 1, None :: r)
        | _ as x -> ( c, x :: r)
    ) in
    (count, row' :: res)
  ) |> snd

let new_game () =
  empty |> add_random |> add_random

type move =
  | Left
  | Right
  | Up
  | Down

let rev = List.map ~f:List.rev

let rec move_atomic = function
  | [] | [_] as l -> l
  | None :: Some x :: tail -> Some x :: move_atomic ( None :: tail)
  | Some x :: Some y :: tail ->
      if x = y then Some (x + y) :: move_atomic (None :: tail)
               else Some x :: Some y :: move_atomic tail
  | Some x :: None :: tail -> Some x :: move_atomic (None ::tail)
  | None :: None :: tail -> move_atomic tail @ [None; None]

let rec move_list l =
  let l' = move_atomic l in
  if l = l'
    then l
    else move_list l'

let move_left = List.map ~f:move_list

let move_right grid = rev grid |> move_left |> rev

let move grid = function
  | Left -> move_left grid
  | Right -> move_right grid
  | Up -> List.transpose_exn grid |> move_left |> List.transpose_exn
  | Down -> List.transpose_exn grid |> move_right |> List.transpose_exn
  |> add_random


let to_llist grid = grid

let to_string grid =
  let strings = List.map grid ~f:(
    List.map ~f:(function
      | None -> ""
      | Some x -> Int.to_string x
  )) in
  let max_len =
    List.max_elt ~cmp:Int.ascending (List.map strings ~f:(fun l ->
      List.max_elt ~cmp:Int.ascending (List.map l ~f:String.length) |> unwrap
    )) |> unwrap in
  let pad x = x ^ String.make (max_len - String.length x) ' ' in
  let strings = List.map strings ~f:(List.map ~f:pad) in
  let strings = List.map strings ~f:(List.intersperse ~sep:" | ") in
  let nb_rows = List.length grid in
  strings
  |> List.intersperse ~sep:([String.make (nb_rows * (max_len + 3)) '-'])
  |> List.intersperse ~sep:(["\n"])
  |> List.concat
  |> String.concat
