open Lib

let try_with_ap s ap =
  let rounds, (final_state, res) =
    s
    |> State.map_units (fun _ _ k u ->
           match k with
           | Elf -> { u with ap }
           | Goblin -> u )
    |> step_to_death_or_completion
    |> seq_enumerate
    |> seq_last
  in

  res |> Result.map (fun _ -> get_output (rounds - 1) final_state)
;;

let () =
  let initial_state = get_input stdin in

  seq_count 4
  |> seq_viewer (fun ap -> Printf.eprintf "%d\n" ap ; flush stderr)
  |> Seq.map (fun ap -> try_with_ap initial_state ap)
  |> seq_stop_after Result.is_ok
  |> seq_last
  |> Result.get_ok
  |> Printf.printf "%d\n"
;;
