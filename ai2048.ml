open Core.Std

let println str =
  Out_channel.output_string stdout str;
  Out_channel.output_string stdout "\n";
  Out_channel.flush stdout

let gen_player ?(depth=5) ?(samples=10) =
  let rec play grid =
    println (Grid.to_string grid);
    println "Grid value: ";
    println (Grid.eval_pos grid |> Float.to_string);
    interact grid
  and interact grid =
    match In_channel.input_line stdin with
      | None -> ()
      | Some line -> match String.to_list line |> List.hd with
        | Some 'h' -> move grid Grid.Left
        | Some 'j' -> move grid Grid.Down
        | Some 'k' -> move grid Grid.Up
        | Some 'l' -> move grid Grid.Right
        | Some 'a' ->
            if String.length line = 1 then
              auto grid ~n:100
            else
              let n = String.slice line 1 0 |> String.strip |> Int.of_string in
              auto grid ~n
        | Some 's' ->
          let suggestions = Brute_force.rank_moves grid ~depth ~samples in
          let move = Brute_force.Move_scores.best_move suggestions in
          println (Brute_force.to_string suggestions);
          println (Grid.move_to_string move);
          interact grid
        | Some 'q' -> println "Exiting."
        | _ -> play grid
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
        let suggestions = Brute_force.rank_moves g ~depth ~samples in
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
  in
  play

let command =
  Command.basic
  ~summary:"Play the 2048 game, with AI help"
  ~readme:(fun () -> "No readme yet")
  Command.Spec.(
    empty
    +> flag "-d"
      (optional_with_default 5 int)
      ~doc:"depth of the brute force AI"
    +> flag "-s"
      (optional_with_default 10 int)
      ~doc:"number of game trees sampled by AI"
  )
  (fun depth samples () ->
    let grid = Grid.new_game () in
    let play = gen_player ~depth ~samples in
    play grid
  )

let () =
  Command.run command
