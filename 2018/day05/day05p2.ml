#! /usr/bin/env ocaml
(* Day 5 Part 2: Alchemical Reduction
 *
 * Usage:
 *     ./day05p2.ml < input.txt
 *
 * Context:
 *
 * Time to improve the polymer.
 *
 * One of the unit types is causing problems; it's preventing the polymer
 * from collapsing as much as it should. Your goal is to figure out which unit
 * type is causing the most problems, remove all instances of it (regardless
 * of polarity), fully react the remaining polymer, and measure its length.
 *
 * For example, again using the polymer dabAcCaCBAcCcaDA from above:
 *
 * - Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer
 *   produces dbCBcD, which has length 6.
 * - Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this
 *   polymer produces daCAcaDA, which has length 8.
 * - Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer
 *   produces daDA, which has length 4.
 * - Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this
 *   polymer produces abCBAc, which has length 6.
 *
 * In this example, removing all C/c units was best, producing the answer 4.
 *
 * What is the length of the shortest polymer you can produce by removing
 * all units of exactly one type and fully reacting the result?
 *)

(* utils *)

let line_seq_of_channel channel =
  let rec seq_of_stream (stream : 'a Stream.t) : 'a Seq.t = fun () ->
    match Stream.peek stream with
    | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
    | None -> Nil
  in
  seq_of_stream
    (Stream.from (fun _ ->
         try Some (input_line channel) with End_of_file -> None))

let rec collapse_polymer (p: string) : string =
  let collapse_once (p: string) : string =
    let rec are_reactive l r : bool =
      not (Char.equal l r)
      &&
      Char.equal (Char.lowercase_ascii l) (Char.lowercase_ascii r)
    and reactive_fold (cum: char list) (next: char) = match cum, next with
      | last::rest, next -> if are_reactive last next then rest else next::last::rest
      | [], n -> [n]
    in
    p
    |> String.to_seq
    |> Seq.fold_left reactive_fold []
    (* List is now in opposite order of string... *)
    |> List.rev
    |> List.to_seq
    |> String.of_seq
  in
  let collapsed = collapse_once p in
  let are_equal = p = collapsed in
  (* Printf.eprintf "%d\t%B\t%s\n" (String.length collapsed) are_equal collapsed; *)
  if are_equal then p else collapse_polymer collapsed

(* inclusive range from s to e *)
let rec char_range (s: char) (e: char) : char Seq.t = fun () ->
  if s > e
  then
    Nil
  else
    Cons (s, char_range (s |> Char.code |> (+) 1 |> Char.chr) e)

let remove_pair (polymer: string) (c: char) : string =
  let c = Char.lowercase_ascii c in
  polymer
  |> String.to_seq
  |> Seq.filter (fun a -> Char.lowercase_ascii a != c)
  |> String.of_seq

let best_removal (p: string) : string =
  char_range 'a' 'z'
  |> Seq.map (remove_pair p)
  |> Seq.map collapse_polymer
  |> List.of_seq
  |> List.sort (fun a b -> Int.compare (String.length a) (String.length b))
  |> List.hd

let main () =
  stdin
  |> line_seq_of_channel
  |> Seq.map best_removal
  (* |> Seq.iter (fun p -> Printf.printf "%s\n" p) *)
  |> Seq.iter (fun p -> Printf.printf "%d\n" (String.length p))

let () =
  main()
