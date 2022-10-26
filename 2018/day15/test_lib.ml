open OUnit2
open Lib

type point = Point.t

let test_point_compare (desc, exp, p1, p2) =
  "test_point_compare_" ^ desc
  >:: fun _ -> assert_equal ~printer:string_of_int exp (Point.compare p1 p2)
;;

let compare_cases : (string * int * point * point) list =
  [ ("equal1", 0, { x= 0; y= 0 }, { x= 0; y= 0 })
  ; ("equal2", 0, { x= 1; y= 2 }, { x= 1; y= 2 })
  ; ("less1", -1, { x= 1; y= 2 }, { x= 2; y= 2 })
  ; ("less2", -1, { x= 1; y= 2 }, { x= 1; y= 3 })
  ]
;;

let suite = "suite" >::: List.map test_point_compare compare_cases

let () = run_test_tt_main suite
