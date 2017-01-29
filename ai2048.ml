open Core.Std

let rec play grid =
  Out_channel.output_string stdout (Grid.to_string grid);
  Out_channel.output_string stdout "\n";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
    | None -> ()
    | Some line -> match String.to_list line |> List.hd with
      | Some 'h' -> move grid Grid.Left
      | Some 'j' -> move grid Grid.Down
      | Some 'k' -> move grid Grid.Up
      | Some 'l' -> move grid Grid.Right
      | _ -> ()
and move grid m =
  let grid = Grid.move grid m in
  play grid

let () =
  let grid = Grid.new_game () in
  play grid
