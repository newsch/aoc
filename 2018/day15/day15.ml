open Lib

let () =
  let state = get_input stdin in
  State.output stderr state
;;
