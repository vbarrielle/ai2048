open Core.Std

let println str =
  Out_channel.output_string stdout str;
  Out_channel.output_string stdout "\n";
  Out_channel.flush stdout


let rec play grid =
  println (Grid.to_string grid);
  println "Grid value: ";
  println (Grid.eval_pos grid |> Float.to_string);
  let suggestions = Brute_force.rank_moves grid ~depth:5 ~samples:40 in
  println (Brute_force.to_string suggestions);
  match In_channel.input_line stdin with
    | None -> ()
    | Some line -> match String.to_list line |> List.hd with
      | Some 'h' -> move grid Grid.Left
      | Some 'j' -> move grid Grid.Down
      | Some 'k' -> move grid Grid.Up
      | Some 'l' -> move grid Grid.Right
      | Some 'a' -> auto grid ~n:10
      | _ -> ()
and move grid m =
  match Grid.move grid m with
    | Grid.Good grid -> play grid
    | Grid.Useless grid ->
        println "This move is useless";
        play grid
    | Grid.Game_over grid ->
        println "Sorry, you lost";
        println (Grid.to_string grid);
and auto grid ~n =
  let rec auto_play g remaining =
    match remaining with
    | 0 -> g
    | _ as n ->
      let suggestions = Brute_force.rank_moves grid ~depth:5 ~samples:10 in
      let move = Brute_force.Move_scores.best_move suggestions in
      println (Brute_force.to_string suggestions);
      println (Grid.move_to_string move);
      match Grid.move g move with
      | Grid.Good grid -> auto_play grid (n - 1)
      | Grid.Useless grid -> assert false
      | Grid.Game_over grid -> assert false
  in
  let res = auto_play grid n in
  play res


let () =
  let grid = Grid.new_game () in
  play grid
