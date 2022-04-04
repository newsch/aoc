#! /usr/bin/env ocaml
(* Day 12: Subterranean Sustainability
 * 
 * 2D Cellular automata
 * 
 * USAGE:
 *
 *     ./day12.ml < input.txt
 *
 * CONTEXT:
 * 
 * The year 518 is significantly more underground than your history books
 * implied. Either that, or you've arrived in a vast cavern network under
 * the North Pole.
 * 
 * After exploring a little, you discover a long tunnel that contains a
 * row of small pots as far as you can see to your left and right. A few
 * of them contain plants - someone is trying to grow things in these
 * geothermally-heated caves.
 * 
 * The pots are numbered, with 0 in front of you. To the left, the pots are
 * numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle
 * input contains a list of pots from 0 to the right and whether they do
 * (#) or do not (.) currently contain a plant, the initial state. (No
 * other pots currently contain plants.) For example, an initial state of
 * #..##.... indicates that pots 0, 3, and 4 currently contain plants.
 * 
 * Your puzzle input also contains some notes you find on a nearby table:
 * someone has been trying to figure out how these plants spread to nearby
 * pots. Based on the notes, for each generation of plants, a given pot has
 * or does not have a plant based on whether that pot (and the two pots on
 * either side of it) had a plant in the last generation. These are written
 * as LLCRR => N, where L are pots to the left, C is the current pot being
 * considered, R are the pots to the right, and N is whether the current
 * pot will have a plant in the next generation. For example:
 * 
 * - A note like ..#.. => . means that a pot that contains a plant but with
 *   no plants within two pots of it will not have a plant in it during the
 *   next generation.
 * - A note like ##.## => . means that an empty pot with two plants on each
 *   side of it will remain empty in the next generation.
 * - A note like .##.# => # means that a pot has a plant in a given generation
 *   if, in the previous generation, there were plants in that pot, the one
 *   immediately to the left, and the one two pots to the right, but not in
 *   the ones immediately to the right and two to the left.
 * 
 * It's not clear what these plants are for, but you're sure it's important, so
 * you'd like to make sure the current configuration of plants is sustainable
 * by determining what will happen after 20 generations.
 * 
 * For example, given the following input:
 * 
 *     initial state: #..#.#..##......###...###
 * 
 *     ...## => #
 *     ..#.. => #
 *     .#... => #
 *     .#.#. => #
 *     .#.## => #
 *     .##.. => #
 *     .#### => #
 *     #.#.# => #
 *     #.### => #
 *     ##.#. => #
 *     ##.## => #
 *     ###.. => #
 *     ###.# => #
 *     ####. => #
 * 
 * For brevity, in this example, only the combinations which do produce a
 * plant are listed. (Your input includes all possible combinations.) Then,
 * the next 20 generations will look like this:
 * 
 *                      1         2         3     
 *            0         0         0         0     
 *      0: ...#..#.#..##......###...###...........
 *      1: ...#...#....#.....#..#..#..#...........
 *      2: ...##..##...##....#..#..#..##..........
 *      3: ..#.#...#..#.#....#..#..#...#..........
 *      4: ...#.#..#...#.#...#..#..##..##.........
 *      5: ....#...##...#.#..#..#...#...#.........
 *      6: ....##.#.#....#...#..##..##..##........
 *      7: ...#..###.#...##..#...#...#...#........
 *      8: ...#....##.#.#.#..##..##..##..##.......
 *      9: ...##..#..#####....#...#...#...#.......
 *     10: ..#.#..#...#.##....##..##..##..##......
 *     11: ...#...##...#.#...#.#...#...#...#......
 *     12: ...##.#.#....#.#...#.#..##..##..##.....
 *     13: ..#..###.#....#.#...#....#...#...#.....
 *     14: ..#....##.#....#.#..##...##..##..##....
 *     15: ..##..#..#.#....#....#..#.#...#...#....
 *     16: .#.#..#...#.#...##...#...#.#..##..##...
 *     17: ..#...##...#.#.#.#...##...#....#...#...
 *     18: ..##.#.#....#####.#.#.#...##...##..##..
 *     19: .#..###.#..#.#.#######.#.#.#..#.#...#..
 *     20: .#....##....#####...#######....#.#..##.
 * 
 * The generation is shown along the left, where 0 is the initial state. The
 * pot numbers are shown along the top, where 0 labels the center pot,
 * negative-numbered pots extend to the left, and positive pots extend toward
 * the right. Remember, the initial state begins at pot 0, which is not the
 * leftmost pot used in this example.
 * 
 * After one generation, only seven plants remain. The one in pot 0 matched
 * the rule looking for ..#.., the one in pot 4 matched the rule looking
 * for .#.#., pot 9 matched .##.., and so on.
 * 
 * In this example, after 20 generations, the pots shown as # contain plants,
 * the furthest left of which is pot -2, and the furthest right of which is
 * pot 34. Adding up all the numbers of plant-containing pots after the 20th
 * generation produces 325.
 * 
 * After 20 generations, what is the sum of the numbers of all pots which
 * contain a plant?
*)

let rec line_seq_of_channel channel = fun () ->
  try
    Seq.Cons (input_line channel, line_seq_of_channel channel)
  with
    End_of_file -> Seq.Nil

let seq_viewer v = Seq.map (fun a -> v a; a);;

(** State of the plant in a pot; true=alive, false=dead *)
type pot_state = bool;;
(** View of 4 pots around a reference pot *)
type pot_window = (pot_state * pot_state * pot_state * pot_state * pot_state);;
(** Determines the state of a pot in the next cycle *)
type pot_rule = (pot_window * pot_state);;
type pot_line = pot_state array;;

type state = {
  pots: pot_line;
  (** index of the '0' pot *)
  offset: int;
}

let parse_input (lines: string Seq.t) : (pot_line * pot_rule list) =
  let rec line = ref 0

  and parse_pot = function
    | '#' -> true
    | '.' -> false
    | c -> failwith (Printf.sprintf "Line %d: Unknown state: '%c'" !line c)

  and parse_initial_state (l: string) : pot_line =
    let state = match String.split_on_char ':' l |> List.map String.trim with
      | ["initial state"; s] -> s
      | _ -> failwith (Printf.sprintf "Line %d: Unable to parse initial state: \"%s\"" !line l)
    in
    state
    |> String.to_seq
    |> Seq.map parse_pot
    |> Array.of_seq

  and parse_rule (l: string) : pot_rule =
    let pattern, result = match String.split_on_char ' ' l |> List.map String.trim with
      | [p; "=>"; r] -> p, r
      | _ -> failwith (Printf.sprintf "Line %d: Unable to parse rule: \"%s\"" !line l)
    in
    let parse_pot_list s = s |> String.to_seq |> Seq.map parse_pot |> List.of_seq in
    (match parse_pot_list pattern with
     | [a;b;c;d;e] -> a, b, c, d, e
     | _ -> failwith (Printf.sprintf "Line %d: Expected 5 elements in rule pattern: \"%s\"" !line l))
    ,
    (match parse_pot_list result with
     | [r] -> r
     | _ -> failwith (Printf.sprintf "Line %d: Expected 1 result in rule pattern: \"%s\"" !line l))
  in

  match lines () with
  | Seq.Nil -> failwith "Expected initial state line"
  | Seq.Cons (l, lines) ->
    line := 1;
    let state = parse_initial_state l
    and rules =
      lines
      |> seq_viewer (fun _ -> line := !line + 1)
      |> Seq.map String.trim
      |> Seq.filter (fun s -> String.compare "" s != 0)
      |> Seq.map parse_rule
      |> Seq.filter (fun (_p, r) -> r = true) (* default is dead, so only look at rules that keep it alive *)
      |> List.of_seq
    in
    state, rules
;;

let output_state out_channel (state: pot_line) =
  let to_char b = if b then '#' else '.' in
  state
  |> Array.to_seq
  |> Seq.map to_char
  |> Seq.iter (output_char out_channel)
;;

let sum_indices_of_alive (s:state) : int =
  s.pots
  |> Array.to_seqi
  |> Seq.filter_map (fun (i, is_alive) -> if is_alive then Some (i - s.offset) else None)
  |> Seq.fold_left (+) 0
;;

let step (s: state) rules : state =
  let determine_state (w: pot_window) : bool =
    rules
    |> List.find_map (fun (w', is_alive) ->
        if compare w w' = 0 then Some is_alive else None)
    |> Option.value ~default:false (* if no rule is found, plant dies *)

  and array_window (a: pot_line): pot_window Seq.t =
    let rec array_window' start fin = fun () ->
      if start > fin then Seq.Nil else
        let try_get i =
          if i < 0 || i >= Array.length a
          then false
          else a.(i)
        in
        Seq.Cons (
          (
            try_get (start - 2),
            try_get (start - 1),
            try_get  start,
            try_get (start + 1),
            try_get (start + 2)
          ),
          array_window' (start + 1) fin)
    in
    array_window' 0 (Array.length a - 1)
  in

  (* grow from left *)
  let s = if Array.get s.pots 0 = true then {
      pots = Array.append [| false; false; |] s.pots;
      offset = s.offset + 2;
    } else s
  in

  (* grow from right *)
  let s = if Array.get s.pots (Array.length s.pots - 1) = true then {
      pots = Array.append s.pots [| false; false; |];
      offset = s.offset;
    } else s
  in

  let new_pots =
    array_window s.pots
    |> Seq.map determine_state
    |> Array.of_seq
  in
  let s = { pots=new_pots; offset=s.offset } in

  s
;;

(** inclusive sequence of start -> stop *)
let rec range start stop: int Seq.t = fun () ->
  if start > stop
  then Seq.Nil
  else Seq.Cons (start, (range (start + 1) stop))
;;

let output_seq ch s =
  output_string ch "[ ";
  Seq.iter (fun el -> output_string ch el; output_string ch "; ") s;
  output_string ch "]";
;;


let () =
  let pots, rules =
    stdin
    |> line_seq_of_channel
    |> parse_input
  in

  let max_offset = 5 in

  (* print header *)
  begin
    let pad = (String.make (4 + max_offset) ' ') ^ "0" in

    output_string stderr pad;
    for i = 1 to Array.length pots / 10 + 1 do
      Printf.fprintf stderr "%10d" i;
    done;
    output_char stderr '\n';

    output_string stderr pad;
    for _ = 1 to Array.length pots / 10 + 1 do
      output_string stderr "         0";
    done;
    output_char stderr '\n';
  end;

  (* first row *)
  Printf.fprintf stderr "%2d: " 0;
  output_string  stderr (String.make max_offset '.');
  output_state   stderr pots;
  output_char    stderr '\n';
  flush stderr;

  let end_state =
    range 1 20
    |> Seq.fold_left (fun state i ->
        let new_state = step state rules in

        (* print the row *)
        if new_state.offset > max_offset then
          (* only a printing issue; does not affect calculation *)
          output_string stderr "WARNING: max_offset not large enough, cols are misaligned\n";
        Printf.fprintf stderr "%2d: " i;
        output_string  stderr (String.make (max 0 (max_offset - new_state.offset)) '.');
        output_state   stderr new_state.pots;
        output_char    stderr '\n';
        flush stderr;

        new_state
      )
      { pots; offset=0; }
  in

  (* Printf.eprintf "Offset:\t%d\n" end_state.offset; *)
  (* end_state.pots *)
  (* |> Array.to_seqi *)
  (* |> Seq.filter (fun (_i, el) -> el = true) *)
  (* |> Seq.map (fun (i, el) -> Printf.sprintf "%d" (i - end_state.offset)) *)
  (* |> output_seq stderr *)
  (* ; *)
  (* output_char stderr '\n'; *)
  (* flush stderr; *)

  Printf.printf "%d\n" (sum_indices_of_alive end_state)
;;

