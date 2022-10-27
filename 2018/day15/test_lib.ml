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

let test_unit_step (desc, unit_num, initial, final) =
  "test_unit_step_" ^ desc
  >:: fun _ ->
  let init = parse initial
  and final = final |> Result.map parse |> Result.map_error parse in
  let unit =
    List.nth (State.units_seq init |> Seq.map fst |> List.of_seq) unit_num
  in
  assert_equal (step_unit unit init) final
;;

let unit_step_tests =
  List.map test_unit_step
    [ ("no_enemies_elf", 0, "E.\n..", Error "E.\n..")
    ; ("no_enemies_gob", 0, "G.\n..", Error "G.\n..")
    ; ("no_open_enemies1", 0, "E.#G", Ok "E.#G")
    ; ("no_open_enemies2", 2, "EE#G", Ok "EE#G")
    ; ("stalemate", 1, "E.#.G", Ok "E.#.G")
    ; ("one_enemy_linear1", 0, "E..G", Ok ".E.G")
    ; ("one_enemy_linear2", 1, "E..G", Ok "E.G.")
    ]
;;

let suite = "suite" >::: compare_tests @ unit_step_tests

let () = run_test_tt_main suite