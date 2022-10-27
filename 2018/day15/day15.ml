open Lib

let () =
  let state = get_input stdin in

  let rounds, (final_state, _, _) =
    state |> step_to_completion |> seq_enumerate |> seq_last
  in

  final_state |> State.output stderr ;

  get_output (rounds - 1) final_state |> Printf.printf "%d\n"
;;
