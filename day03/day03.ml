#! /usr/bin/env ocaml
open Printf
open Seq


(* utils *)

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

(* meat & potatoes *)

type fabric = {
  width: int;
  height: int;
  vals: int Array.t;
}

let fabric_make (w:int) (h:int) : fabric = {
  width = w;
  height = h;
  vals = Array.make (w*h) 0;
}

type claim = {
  x: int;
  y: int;
  w: int;
  h: int;
}

let add_claim (f:fabric) (c:claim) =
  let add_point f x y =
    let i = f.width * y + x in
    Array.set f.vals i ((Array.get f.vals i) + 1)
  in
  let { x; y; w; h; } = c in
  for y' = y to y+h-1 do
    for x' = x to x+w-1 do
      add_point f x' y'
    done
  done

let parse_claim s : claim Option.t =
  (* format: #<id> @ <x>,<y>: <w>x<h> *)
  match String.split_on_char '@' s with
  | [_;s] ->
    (match String.split_on_char ':' s with
    | [p;d] -> (
      match p |> String.trim |> String.split_on_char ',' with
      | [x; y] -> (
        match d |> String.trim |> String.split_on_char 'x' with
        | [w; h] -> (
          let [x;y;w;h] = List.map int_of_string [x;y;w;h] in
          Some { x; y; w; h;}
        )
        | _ -> None
      )
      | _ -> None
    )
    | _ -> None)
  | _ -> None


let () =
  let f = fabric_make 1000 1000 in
  let line_count = ref 0 in

  stdin
  |> line_seq_of_channel
  |> Seq.map parse_claim
  |> Seq.map (fun m -> line_count := !line_count + 1; m)
  |> Seq.iter (function
    | Some c -> add_claim f c;
    | None -> Printf.sprintf "couldn't parse line %d" !line_count |> failwith);

  Array.to_seq f.vals
  |> Seq.filter (fun n -> n > 1)
  |> Seq.fold_left (fun s _ -> s + 1) 0
  |> Printf.printf "%d\n";
