open Core.Std

module Move_scores = struct
  type t =
    { left: float;
      right: float;
      up: float;
      down: float;
    }
end

type 'a move_tree =
  | Node of 'a move_tree * 'a move_tree * 'a move_tree * 'a move_tree
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
           Node (left, up, right, down)
  in
  do_moves (Grid.Good grid) depth

let rec tree_depth = function
  | Final _ -> 0
  | Node (left, right, up, down) ->
      tree_depth left
      |> Int.max (tree_depth right)
      |> Int.max (tree_depth up)
      |> Int.max (tree_depth down)
      |> (+) 1

let rank_moves grid depth =
  let grids = enumerate_moves grid depth in
  let () = assert (tree_depth grids > 0) in
  (* recursively transform the grid tree into a score tree,
   * ensuring one level remains *)
  let rec rank = function
    | Final (Grid.Good grid) -> Grid.eval_pos grid
    | Final (Grid.Useless grid) -> Grid.eval_pos grid
    | Final (Grid.Game_over grid) -> -2048.
    | Node (left, right, up, down)
      -> rank left
      |> Float.max (rank right)
      |> Float.max (rank up)
      |> Float.max (rank down)
  in
  match grids with
  | Final _ -> assert false
  | Node (left, right, up, down)
    -> { Move_scores.left = rank left;
                     right = rank right;
                     up = rank up;
                     down = rank down; }

let to_string {Move_scores.left; right; up; down} =
  sprintf "left: %f, right: %f, up: %f, down: %f" left right up down
