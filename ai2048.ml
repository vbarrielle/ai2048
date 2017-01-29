open Core.Std

let rec play grid =
  let () = printf "%s\n" (Grid.to_string grid) in
  let move = match In_channel.input_char stdin with
    | Some 'h' -> Grid.Left
    | Some 'j' -> Grid.Down
    | Some 'k' -> Grid.Up
    | Some 'l' -> Grid.Right
    | _ -> assert false
  in
  let grid = Grid.move grid move in
  play grid

let () =
  let grid = Grid.new_game () in
  play grid
