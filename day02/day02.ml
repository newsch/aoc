#! /usr/bin/env ocaml
open Printf
open Seq


let rec seq_of_stream stream =
  (fun () ->
    match Stream.peek stream with
    | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
    | None -> Nil
  )

let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None);;

let line_seq_of_channel channel = seq_of_stream (line_stream_of_channel channel)

let group (xs: 'x list) : 'x list list =
  let rec group' (acc: 'x list list) (xs': 'x list) = match xs' with
    | [] -> acc
    | hd::tl ->
      let same, diff = List.partition (fun x -> hd = x) tl in
      group' ((hd::same)::acc) diff
    in
    group' [] xs

let count_occurances (xs: 'x list) : int list =
  List.map List.length (group xs)

let explode s = List.init (String.length s) (String.get s)

let calculate_checksum (ids: string t) : int =
  let check_appearances (ids: string t) : int * int =
    let has n s = Option.is_some (List.find_opt (fun n' -> n' = n) s) in
    Seq.fold_left
      (fun (d, t) id ->
        let apps = count_occurances (explode id) in
        (
          (if has 2 apps then d + 1 else d),
          (if has 3 apps then t + 1 else t)
        )
      )
    (0, 0)
    ids
  in
  let (doubles, triples) = check_appearances ids in doubles * triples

let () =
  let cs = calculate_checksum (line_seq_of_channel stdin) in
  Printf.printf "%d\n" cs;
  flush stdout;;
