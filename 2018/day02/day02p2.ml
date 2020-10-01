#! /usr/bin/env ocaml
open Printf
open Seq


let rec seq_of_stream stream =
  (fun () ->
    match Stream.peek stream with
    | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
    | None -> Nil
  )

let line_seq_of_channel channel =
  seq_of_stream
    (Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None))

let rec seq_append seq1 seq2 () =
  match seq1() with
  | Nil -> seq2()
  | Cons (x, next) -> Cons (x, append next seq2)

let explode s = List.init (String.length s) (String.get s)

let zip = List.map2 (fun a' b' -> (a', b'))

let string_of_list s : string = String.of_seq (List.to_seq s)

let rec combinations a =
  match a with
  | hd::tl -> seq_append
    (Seq.map (fun a' -> (hd, a')) (List.to_seq tl))
    (combinations tl)
  | [] -> Seq.empty

let string_diff a b : int =
  zip (explode a) (explode b)
  |> List.filter (fun (a', b') -> a' <> b')
  |> List.length

let keep_matching a b =
  let cmp_opt a b = if a = b then Some a else None in
  zip a b
  |> List.filter_map (fun (a', b') -> cmp_opt a' b')

let () =
  stdin
  |> line_seq_of_channel
  |> List.of_seq
  |> combinations
  |> Seq.filter (fun (a, b) -> string_diff a b = 1)
  (* |> Seq.iter (fun (a, b) -> (Printf.printf "%s\t%s\n" a b)); *)
  |> Seq.map (fun (a, b) -> keep_matching (explode a) (explode b))
  |> Seq.map string_of_list
  |> Seq.iter (fun s -> Printf.printf "%s\n" s);
  flush stdout;;
