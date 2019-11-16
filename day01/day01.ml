#! /usr/bin/env ocaml
open Printf


let apply_cmd (total: int) (c: string) : int =
  let v = int_of_string (String.sub c 1 ((String.length c) - 1)) in
  match String.get c 0 with
    '+' -> total + v
  | '-' -> total - v
  | _ -> failwith "invalid cmd";;


let () =
  let lines = ref [] in
  try
    while true; do
      lines := input_line stdin :: !lines
    done;
  with End_of_file ->
    (* print_endline (List.fold_left (^) "" !lines); *)
    Printf.printf "%d\n" (List.fold_left apply_cmd 0 !lines);
    flush stdout;;
