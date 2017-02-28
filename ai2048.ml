open Core.Std

let rec play grid =
  Out_channel.output_string stdout (Grid.to_string grid);
  Out_channel.output_string stdout "\n";
  Out_channel.output_string stdout "Grid value: ";
  Out_channel.output_string stdout (Grid.eval_pos grid |> Float.to_string);
  Out_channel.output_string stdout "\n";
  let suggestions = Brute_force.rank_moves grid 5 in
  Out_channel.output_string stdout (Brute_force.to_string suggestions);
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
  match Grid.move grid m with
    | Grid.Good grid -> play grid
    | Grid.Useless grid ->
        Out_channel.output_string stdout "This move is useless\n";
        play grid
    | Grid.Game_over grid ->
        Out_channel.output_string stdout "Sorry, you lost\n";
        Out_channel.output_string stdout (Grid.to_string grid);
        Out_channel.output_string stdout "\n"

let () =
  let grid = Grid.new_game () in
  play grid
