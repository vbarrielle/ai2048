open Core.Std

type 'a moves = {left: 'a; right: 'a; up: 'a; down: 'a}

module Move_scores = struct
  type t = float moves

  let (+) lhs rhs =
    {
      left = lhs.left +. rhs.left;
      right = lhs.right +. rhs.right;
      up = lhs.up +. rhs.up;
      down = lhs.down +. rhs.down;
    }

  let (/) scores scalar =
    {
      left = scores.left /. scalar;
      right = scores.right /. scalar;
      up = scores.up /. scalar;
      down = scores.down /. scalar;
    }

  let best_move { left; right; up; down } =
    let max_score = left |> Float.max right |> Float.max up |> Float.max down in
    if left = max_score then
      Grid.Left
    else if down = max_score then
      Grid.Down
    else if right = max_score then
      Grid.Right
    else if up = max_score then
      Grid.Up
    else
      assert false
end

type 'a move_tree =
  | Node of ('a move_tree) moves
  | Final of 'a

let enumerate_moves grid depth =
  let () = assert (depth > 0) in
  let rec do_moves grid level =
    if level = 0 then
      Final grid
    else
      let level = level - 1 in
      match grid with
      | Grid.Game_over grid -> Final (Grid.Game_over grid)
      | Grid.Useless grid -> Final (Grid.Useless grid)
      | Grid.Good grid
        -> let left = do_moves (Grid.move grid Grid.Left) level in
           let right = do_moves (Grid.move grid Grid.Right) level in
           let up = do_moves (Grid.move grid Grid.Up) level in
           let down = do_moves (Grid.move grid Grid.Down) level in
           Node {left; right; up; down}
  in
  do_moves (Grid.Good grid) depth

let rec tree_depth = function
  | Final _ -> 0
  | Node {left; right; up; down} ->
      tree_depth left
      |> Int.max (tree_depth right)
      |> Int.max (tree_depth up)
      |> Int.max (tree_depth down)
      |> (+) 1

(** Rank a game tree as the value of the best future grid *)
let rec rank_tree_max = function
  | Final (Grid.Good grid) -> Grid.eval_pos grid
  | Final (Grid.Useless grid) -> -4096.
  | Final (Grid.Game_over grid) -> -2048.
  | Node {left; right; up; down} ->
    rank_tree_max left
    |> Float.max (rank_tree_max right)
    |> Float.max (rank_tree_max up)
    |> Float.max (rank_tree_max down)

let rank_tree_mean2max = function
  | Final (Grid.Good grid) -> Grid.eval_pos grid
  | Final (Grid.Useless grid) -> -4096.
  | Final (Grid.Game_over grid) -> -2048.
  | Node {left; right; up; down} ->
    let sorted_scores =
      List.map [left; right; up; down] ~f:rank_tree_max
      |> List.sort ~cmp:Float.compare
      |> List.rev
    in
    let best = List.hd_exn sorted_scores in
    let snd = List.hd_exn @@ List.tl_exn sorted_scores in
    Float.((best + snd) * 0.5)

let rec rank_tree_max_empty = function
  | Final (Grid.Good grid) -> Float.of_int @@ Grid.nb_empty grid
  | Final (Grid.Useless grid) -> -4096.
  | Final (Grid.Game_over grid) -> -2048.
  | Node {left; right; up; down} ->
    rank_tree_max_empty left
    |> Float.max (rank_tree_max_empty right)
    |> Float.max (rank_tree_max_empty up)
    |> Float.max (rank_tree_max_empty down)

let rank_moves_sample grid depth =
  let grids = enumerate_moves grid depth in
  let () = assert (tree_depth grids > 0) in
  let rank = rank_tree_max_empty in
  match grids with
  | Final _ -> assert false
  | Node {left; right; up; down} ->
    { left = rank left; right = rank right; up = rank up; down = rank down; }

let rank_moves grid ~depth ~samples =
  let rankings = List.init samples ~f:(fun _ -> rank_moves_sample grid depth) in
  let sum = List.fold
    ~init:{left = 0.; right = 0.; up = 0.; down = 0.;}
    ~f:Move_scores.(+)
    rankings
  in
  Move_scores.(sum / (Float.of_int samples))

let to_string {left; right; up; down} =
  sprintf "left: %f, right: %f, up: %f, down: %f" left right up down
