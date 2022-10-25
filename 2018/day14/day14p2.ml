#! /usr/bin/env ocaml

(* Day 14: Chocolate Charts
 *
 * USAGE:
 *
 * ./day14p2.ml [-v] < input.txt 
 *
 * CONTEXT:
 *
 * As it turns out, you got the Elves' plan backwards. They actually want to
 * know how many recipes appear on the scoreboard to the left of the first
 * recipes whose scores are the digits from your puzzle input.
 * 
 *     51589 first appears after 9 recipes.
 *     01245 first appears after 5 recipes.
 *     92510 first appears after 18 recipes.
 *     59414 first appears after 2018 recipes.
 * 
 * How many recipes appear on the scoreboard to the left of the score sequence
 * in your puzzle input?
 *)

type recipe = int

let digits (n : int) : int list =
  let rec digits' n =
    let lowest_digit = n mod 10 and remaining = n / 10 in
    lowest_digit :: (if remaining = 0 then [] else digits' remaining)
  in
  digits' n |> List.rev
;;

(** List backed by a Map for efficient pushes and indexing *)
module MapList = struct
  module IntMap = Map.Make (Int)

  type 'a t = { length: int; map: 'a IntMap.t }

  let empty = { length= 0; map= IntMap.empty }

  let push i m =
    let map = IntMap.add m.length i m.map and length = m.length + 1 in
    { map; length }
  ;;

  let rec push_seq (s : 'a Seq.t) (m : 'a t) =
    match s () with
    | Seq.Nil -> m
    | Seq.Cons (el, rest) -> push_seq rest (push el m)
  ;;

  let rec nth m i =
    if i < 0 || i >= m.length then failwith "Index out of range"
    else IntMap.find i m.map
  ;;

  let to_seq m =
    let rec to_seq' i () =
      if i >= m.length then Seq.Nil
      else Seq.Cons (IntMap.find i m.map, to_seq' (i + 1))
    in
    to_seq' 0
  ;;

  let of_list l = push_seq (List.to_seq l) empty

  let length m = m.length
end

let combine_recipes (r1 : recipe) (r2 : recipe) : recipe list = digits (r1 + r2)

type state = { elves: int list; scoreboard: recipe MapList.t }

let output_state ch { elves; scoreboard } =
  let get_surrounds i =
    elves
    |> List.mapi (fun elf_num elf_ind ->
           if elf_ind = i then Some elf_num else None )
    |> List.filter_map (fun e -> e)
    |> function
    | [] -> (' ', ' ')
    | 0 :: _ -> ('(', ')')
    | 1 :: _ -> ('[', ']')
    | _ -> ('?', '?')
  in
  let output_score index value =
    let left, right = get_surrounds index in
    output_char ch left ;
    output_string ch (string_of_int value) ;
    output_char ch right
  in
  scoreboard |> MapList.to_seq |> Seq.iteri output_score ;
  output_char ch '\n'
;;

let step (s : state) : state * recipe list =
  let recipes = s.elves |> List.map (MapList.nth s.scoreboard) in
  let new_recipes = combine_recipes (List.nth recipes 0) (List.nth recipes 1) in
  let scoreboard = MapList.push_seq (List.to_seq new_recipes) s.scoreboard in

  let new_elf elf_num recipe_num : int =
    let advance = MapList.nth s.scoreboard recipe_num + 1 in
    let new_recipe = (recipe_num + advance) mod MapList.length scoreboard in
    new_recipe
  in

  let elves = s.elves |> List.mapi new_elf in

  ({ elves; scoreboard }, new_recipes)
;;

let step_forever initial_state =
  let initial_recipes =
    initial_state.scoreboard |> MapList.to_seq |> List.of_seq
  in
  let rec step_forever' (s, new_recipes) () =
    Seq.Cons ((s, new_recipes), step_forever' (step s))
  in
  step_forever' (initial_state, initial_recipes)
;;

let initial_state = { elves= [ 0; 1 ]; scoreboard= MapList.of_list [ 3; 7 ] }

let get_input ch : int list =
  let line = input_line ch |> String.trim in
  List.init (String.length line) (fun i ->
      String.get line i |> int_of_char |> ( + ) (-48) )
;;

let seq_viewer v = Seq.map (fun a -> v a ; a)

let seq_viewer_if c v s = if c then seq_viewer v s else s

(** Get the last non-Nil item in a sequence *)
let seq_last (s : 'a Seq.t) : 'a =
  let rec seq_last' last (s : 'a Seq.t) =
    match s () with
    | Nil -> last
    | Cons (last', s') -> seq_last' last' s'
  in
  match s () with
  | Nil -> failwith "Sequence was empty"
  | Cons (last, s') -> seq_last' last s'
;;

let verbose : bool =
  Sys.argv
  |> Array.to_seq
  |> Seq.drop 1
  |> Seq.find (( = ) "-v")
  |> Option.is_some
;;

let find_in_seq (l : int list) (s : int Seq.t) : int =
  let rec starts_with l' s' =
    match (l', s' ()) with
    | [], _ -> true
    | a :: rl, Seq.Cons (b, rs) -> if a = b then starts_with rl rs else false
    | _ -> false
  in
  let rec find_in_seq' i s' =
    match s' () with
    | Seq.Nil -> failwith "Reached end of sequence"
    | Seq.Cons (_, rs) ->
        if starts_with l s' then i else find_in_seq' (i + 1) rs
  in
  find_in_seq' 0 (s |> Seq.memoize)
;;

let () =
  let pattern = get_input stdin in

  if verbose then
    Printf.eprintf "Searching for %s ...\n"
      (pattern |> List.map string_of_int |> String.concat " ") ;

  let start_index =
    step_forever initial_state
    (* |> seq_viewer_if verbose (fun (s, _) -> output_state stderr s) *)
    |> Seq.map snd
    |> Seq.flat_map List.to_seq
    |> seq_viewer_if verbose (Printf.eprintf "%d")
    |> find_in_seq pattern
  in

  if verbose then output_char stderr '\n' ;
  flush stderr ;

  Printf.printf "%d\n" start_index
;;
