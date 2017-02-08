
type 'a move_tree =
  | Node of 'a * 'a move_tree * 'a move_tree * 'a move_tree * 'a move_tree
  | Final of 'a

let enumerate_moves grid depth =
  assert depth > 0 in
  let rec do_moves grid level =
    if level = 0 then
      Final grid
    else
      let level = level - 1 in
      match grid with
        | Err _ as grid -> Final grid
        | Some _ as grid -> Node (grid,
                             do_moves (Grid.move grid Grid.Left) level,
                             do_moves (Grid.move grid Grid.Right) level,
                             do_moves (Grid.move grid Grid.Up) level,
                             do_moves (Grid.move grid Grid.Down) level)
  in
  do_moves (Ok grid) depth

let rank_moves grid depth =
  let grids = enumerate_moves grid depth in
  (* recursively transform the grid tree into a score tree,
   * ensuring one level remains *)
  assert false
