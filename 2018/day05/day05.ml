#! /usr/bin/env ocaml
(* Day 5: Alchemical Reduction
 *
 * Usage:
 *     ./day05.ml < input.txt
 *
 * Context:
 *
 * ...
 * The polymer is formed by smaller units which, when triggered, react with each
 * other such that two adjacent units of the same type and opposite polarity
 * are destroyed. Units' types are represented by letters; units' polarity is
 * represented by capitalization. For instance, r and R are units with the same
 * type but opposite polarity, whereas r and s are entirely different types
 * and do not react.
 *
 * The polymer is formed by smaller units which, when triggered, react with each
 * other such that two adjacent units of the same type and opposite polarity
 * are destroyed. Units' types are represented by letters; units' polarity is
 * represented by capitalization. For instance, r and R are units with the same
 * type but opposite polarity, whereas r and s are entirely different types
 * and do not react.
 *
 * For example:
 *
 * - In aA, a and A react, leaving nothing behind.
 * - In abBA, bB destroys itself, leaving aA. As above, this then destroys
 *   itself, leaving nothing.
 * - In abAB, no two adjacent units are of the same type, and so nothing happens.
 * - In aabAAB, even though aa and AA are of the same type, their polarities
 *   match, and so nothing happens.
 *
 * Now, consider a larger example, dabAcCaCBAcCcaDA:
 *
 *     dabAcCaCBAcCcaDA  The first 'cC' is removed.
 *     dabAaCBAcCcaDA    This creates 'Aa', which is removed.
 *     dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
 *     dabCBAcaDA        No further actions can be taken.
 *
 * After all possible reactions, the resulting polymer contains 10 units.
 *
 * How many units remain after fully reacting the polymer you scanned?
 * (Note: in this puzzle and others, the input is large; if you copy/paste your
 * input, make sure you get the whole thing.)
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

let main () =
  stdin
  |> line_seq_of_channel
  |> Seq.map collapse_polymer
  (* |> Seq.iter (fun p -> Printf.printf "%s\n" p) *)
  |> Seq.iter (fun p -> Printf.printf "%d\n" (String.length p))

let () =
  main()
