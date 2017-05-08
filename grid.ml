open Core.Std
open Utils

type grid = int option list list
type t = {grid: grid; score: float}

type content =
  | Empty
  | Free of int
  | Locked of int

let grid_size = 4

let empty =
  [ [ None; None; None; None ];
    [ None; None; None; None ];
    [ None; None; None; None ];
    [ None; None; None; None ]; ]

let map grid ~f =
  List.map grid ~f:(fun row -> List.map row ~f)

let fold grid ~init ~f =
  List.fold grid ~init ~f:(fun acc row -> List.fold row ~init:acc ~f)

let to_content grid =
  map grid ~f:(function
    | Some x -> Free x
    | None -> Empty
  )

let from_content grid =
  map grid ~f:(function
    | Empty -> None
    | Free x -> Some x
    | Locked x -> Some x
  )


let sample_new () =
  if Random.float 1. <. 0.9 then Some 2 else Some 4

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

let empty_locations {grid} =
  fold grid ~init:(0, []) ~f:(fun (id, locs) elem ->
    match elem with
    | None -> (id + 1, id::locs)
    | _ -> (id + 1, locs)
  ) |> snd

let new_game () =
  let grid = empty |> add_random |> add_random in
  {grid; score = 0.0 }

type move =
  | Left
  | Right
  | Up
  | Down

type move_res =
  | Good of t
  | Useless of t
  | Game_over of t

let rev = List.map ~f:List.rev

let rec move_atomic = function
  | [] | [_] as l -> l
  | Empty :: tail -> (move_atomic tail) @ [Empty]
  | Free x :: Free y :: tail ->
      if x = y then Locked (x + y) :: move_atomic (Empty :: tail)
               else Free x :: move_atomic (Free y :: tail)
  | Free x :: tail -> Free x :: move_atomic tail
  | Locked x :: tail -> Locked x :: move_atomic tail

let rec move_list l =
  let l' = move_atomic l in
  if l = l'
    then l
    else move_list l'

let move_left = List.map ~f:move_list

let move_right grid = rev grid |> move_left |> rev

let move_pure grid m =
  let grid = to_content grid in
  let grid = match m with
  | Left -> move_left grid
  | Right -> move_right grid
  | Up -> List.transpose_exn grid |> move_left |> List.transpose_exn
  | Down -> List.transpose_exn grid |> move_right |> List.transpose_exn
  in
  let merged_scores = fold grid ~init:0 ~f:(fun acc value ->
    match value with
    | Locked x -> acc + x
    | _ -> acc
  ) in
  from_content grid, (Float.of_int merged_scores)

let game_over grid =
  move_pure grid Left |> fst = grid &&
  move_pure grid Right |> fst = grid &&
  move_pure grid Up |> fst = grid &&
  move_pure grid Down |> fst = grid

let move {grid; score} m =
  let moved, merged_scores = move_pure grid m in
  if moved = grid then
    Useless {grid = moved; score}
  else
    let res = add_random moved in
    if game_over res then
      Game_over {grid = res; score = score +. merged_scores}
    else
      Good {grid = res; score = score +. merged_scores}

let to_llist {grid; score} = grid

let to_string {grid; score} =
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

let nb_full grid =
  fold grid ~init:0 ~f:(fun acc x ->
    match x with
      | Some _ -> acc + 1
      | None -> acc
  )

let nb_empty {grid; score} =
  let grid_size = List.length grid in
  (grid_size * grid_size) - (nb_full grid)

let heuristic_eval_pos {grid; score} =
  let sum_cases = fold grid ~init:0 ~f:(fun acc x ->
    match x with
      | Some y -> acc + y
      | None -> acc
  ) in
  let nb_cases = nb_full grid in
  Float.of_int sum_cases /. Float.of_int nb_cases

let eval_pos {score; } =
  score

let move_to_string = function
  | Left -> "Left"
  | Right -> "Right"
  | Down -> "Down"
  | Up -> "Up"

let highest {grid; score} =
  fold grid ~init:0 ~f:(fun max x ->
    match x with
    | Some y -> Int.max max y
    | None -> max
  )
