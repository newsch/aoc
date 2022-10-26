open Lib

let () =
  let state = get_input stdin in
  output_state stderr state
;;
