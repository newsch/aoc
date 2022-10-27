open OUnit2
open Lib

type point = Point.t

let test_point_compare (desc, exp, p1, p2) =
  "test_point_compare_" ^ desc
  >:: fun _ -> assert_equal ~printer:string_of_int exp (Point.compare p1 p2)
;;

let compare_tests =
  List.map test_point_compare
    [ ("equal1", 0, { x= 0; y= 0 }, { x= 0; y= 0 })
    ; ("equal2", 0, { x= 1; y= 2 }, { x= 1; y= 2 })
    ; ("less1", -1, { x= 1; y= 2 }, { x= 2; y= 2 })
    ; ("less2", -1, { x= 1; y= 2 }, { x= 1; y= 3 })
    ]
;;

let parse s = s |> char_seq_of_string |> State.parse

let test_unit_step (desc, unit_num, initial, outcome, final) =
  "test_unit_step_" ^ desc
  >:: fun _ ->
  let unit =
    List.nth (State.units_seq initial |> Seq.map fst |> List.of_seq) unit_num
  in

  let got_state, got_outcome = step_unit unit initial in
  assert_equal got_state final ;
  assert_equal got_outcome outcome
;;

let unit_step_tests =
  List.map test_unit_step
    [ ("no_enemies_elf", 0, parse "E.\n..", NoEnemies, parse "E.\n..")
    ; ("no_enemies_gob", 0, parse "G.\n..", NoEnemies, parse "G.\n..")
    ; ("no_open_enemies1", 0, parse "E.#G", NoFreeSpaces, parse "E.#G")
    ; ("no_open_enemies2", 2, parse "EE#G", NoFreeSpaces, parse "EE#G")
    ; ("no_path", 1, parse "E.#.G", NoPath, parse "E.#.G")
    ; ("one_enemy_linear1", 0, parse "E..G", Moved, parse ".E.G")
    ; ("one_enemy_linear2", 1, parse "E..G", Moved, parse "E.G.")
    ]
;;

let suite = "suite" >::: compare_tests @ unit_step_tests

let () = run_test_tt_main suite
