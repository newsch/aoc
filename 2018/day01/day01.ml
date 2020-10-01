#! /usr/bin/env ocaml
open Printf


let stream_fold f init stream =
  let result = ref init in
  Stream.iter
    (fun x -> result := f !result x)
    stream;
  !result;;

let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None);;


let apply_cmd (total: int) (c: string) : int =
  let v = int_of_string (String.sub c 1 ((String.length c) - 1)) in
  match String.get c 0 with
    '+' -> total + v
  | '-' -> total - v
  | _ -> failwith "invalid cmd";;


let () =
  Printf.printf "%d\n" (stream_fold apply_cmd 0 (line_stream_of_channel stdin));
  flush stdout;;
