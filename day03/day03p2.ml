#! /usr/bin/env -S ocaml str.cma
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
  vals: int list Array.t;
}

let fabric_make (w:int) (h:int) : fabric = {
  width = w;
  height = h;
  vals = Array.make (w*h) [];
}

type claim = {
  id: int;
  x: int;
  y: int;
  w: int;
  h: int;
}

let add_claim (f:fabric) (c:claim) =
  let add_point f x y =
    let i = f.width * y + x in
    Array.set f.vals i (c.id::(Array.get f.vals i))
  in
  let { x; y; w; h; } = c in
  for y' = y to y+h-1 do
    for x' = x to x+w-1 do
      add_point f x' y'
    done
  done

type parse_claim_error =
  | RegexFail
  | BadId
  | BadX
  | BadY
  | BadW
  | BadH

let parse_claim_error_to_string = function
  | RegexFail -> "RegexFail"
  | BadId -> "BadId"
  | BadX -> "BadX"
  | BadY -> "BadY"
  | BadW -> "BadW"
  | BadH -> "BadH"

let claim_r = Str.regexp "^#\\([0-9]+\\) @ \\([0-9]+\\),\\([0-9]+\\): \\([0-9]+\\)x\\([0-9]+\\)$"
let parse_claim s : (claim, parse_claim_error) Result.t =
  (* format: #<id> @ <x>,<y>: <w>x<h> *)
  if Str.string_match claim_r s 0 then
    let get_piece i = int_of_string_opt (Str.matched_group i s) in
    match get_piece 1 with
    | None -> Error BadId
    | Some id -> (
      match get_piece 2 with
      | None -> Error BadX
      | Some x -> (
        match get_piece 3 with
        | None -> Error BadY
        | Some y -> (
          match get_piece 4 with
          | None -> Error BadW
          | Some w -> (
            match get_piece 5 with
            | None -> Error BadH
            | Some h -> Ok { id; x; y; w; h; }))))
  else Error RegexFail

module IdSet = Set.Make(Int)

let () =
  let f = fabric_make 1000 1000 in
  let line_count = ref 0 in

  let ids : IdSet.t =
    stdin
    |> line_seq_of_channel
    |> Seq.map parse_claim
    |> Seq.map (fun m -> line_count := !line_count + 1; m)
    |> Seq.map (function
      | Ok c -> c
      | Error e -> Printf.sprintf "couldn't parse line %d: %s" !line_count (parse_claim_error_to_string e) |> failwith)
    |> Seq.map (fun c -> add_claim f c; c)
    |> Seq.map (fun c -> c.id)
    |> (fun i -> IdSet.add_seq i IdSet.empty)
  in

  let overlapping: IdSet.t =
    Array.to_seq f.vals
    |> Seq.filter (fun ids' -> List.length ids' > 1)
    |> Seq.flat_map List.to_seq
    |> (fun ids' -> IdSet.add_seq ids' IdSet.empty)
  in

  IdSet.diff ids overlapping
  |> IdSet.iter (Printf.printf "%d\n")
