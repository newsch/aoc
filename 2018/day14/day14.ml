(* #! /usr/bin/env ocaml *)

(* Day 14: Chocolate Charts
 *
 * USAGE:
 *
 * ./day14.ml [-v] < input.txt 
 *
 * CONTEXT:
 *
 * You finally have a chance to look at all of the produce moving
 * around. Chocolate, cinnamon, mint, chili peppers, nutmeg, vanilla... the
 * Elves must be growing these plants to make hot chocolate! As you realize
 * this, you hear a conversation in the distance. When you go to investigate,
 * you discover two Elves in what appears to be a makeshift underground
 * kitchen/laboratory.
 * 
 * The Elves are trying to come up with the ultimate hot chocolate recipe;
 * they're even maintaining a scoreboard which tracks the quality score
 * (0-9) of each recipe.
 * 
 * Only two recipes are on the board: the first recipe got a score of 3, the
 * second, 7. Each of the two Elves has a current recipe: the first Elf starts
 * with the first recipe, and the second Elf starts with the second recipe.
 * 
 * To create new recipes, the two Elves combine their current recipes. This
 * creates new recipes from the digits of the sum of the current recipes'
 * scores. With the current recipes' scores of 3 and 7, their sum is 10,
 * and so two new recipes would be created: the first with score 1 and the
 * second with score 0. If the current recipes' scores were 2 and 3, the sum,
 * 5, would only create one recipe (with a score of 5) with its single digit.
 * 
 * The new recipes are added to the end of the scoreboard in the order they
 * are created. So, after the first round, the scoreboard is 3, 7, 1, 0.
 * 
 * After all new recipes are added to the scoreboard, each Elf picks a new
 * current recipe. To do this, the Elf steps forward through the scoreboard a
 * number of recipes equal to 1 plus the score of their current recipe. So,
 * after the first round, the first Elf moves forward 1 + 3 = 4 times,
 * while the second Elf moves forward 1 + 7 = 8 times. If they run out of
 * recipes, they loop back around to the beginning. After the first round,
 * both Elves happen to loop around until they land on the same recipe that
 * they had in the beginning; in general, they will move to different recipes.
 * 
 * Drawing the first Elf as parentheses and the second Elf as square brackets,
 * they continue this process:
 * 
 *     (3)[7]
 *     (3)[7] 1  0 
 *      3  7  1 [0](1) 0 
 *      3  7  1  0 [1] 0 (1)
 *     (3) 7  1  0  1  0 [1] 2 
 *      3  7  1  0 (1) 0  1  2 [4]
 *      3  7  1 [0] 1  0 (1) 2  4  5 
 *      3  7  1  0 [1] 0  1  2 (4) 5  1 
 *      3 (7) 1  0  1  0 [1] 2  4  5  1  5 
 *      3  7  1  0  1  0  1  2 [4](5) 1  5  8 
 *      3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 *      3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6 
 *      3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7 
 *      3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7 
 *      3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9 
 *      3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2 
 * 
 * The Elves think their skill will improve after making a few recipes
 * (your puzzle input). However, that could take ages; you can speed this up
 * considerably by identifying the scores of the ten recipes after that. For
 * example:
 * 
 *     If the Elves think their skill will improve after making 9 recipes,
 *     the scores of the ten recipes after the first nine on the scoreboard
 *     would be 5158916779 (highlighted in the last line of the diagram).
 *     After 5 recipes, the scores of the next ten would be 0124515891.
 *     After 18 recipes, the scores of the next ten would be 9251071085.
 *     After 2018 recipes, the scores of the next ten would be 5941429882.
 * 
 * What are the scores of the ten recipes immediately after the number of
 * recipes in your puzzle input?
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
  type 'a t = { length: int; map: (int, 'a) Hashtbl.t }

  let empty = { length= 0; map= Hashtbl.create 0 }

  let push i m =
    Hashtbl.add m.map m.length i ;
    { m with length= m.length + 1 }
  ;;

  let rec push_seq (s : 'a Seq.t) (m : 'a t) =
    match s () with
    | Seq.Nil -> m
    | Seq.Cons (el, rest) -> push_seq rest (push el m)
  ;;

  let rec nth m i =
    if i < 0 || i >= m.length then failwith "Index out of range"
    else Hashtbl.find m.map i
  ;;

  let to_seq m =
    let rec to_seq' i () =
      if i >= m.length then Seq.Nil
      else Seq.Cons (Hashtbl.find m.map i, to_seq' (i + 1))
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

let step (s : state) : state =
  let recipes = s.elves |> List.map (MapList.nth s.scoreboard) in
  let new_recipes = combine_recipes (List.nth recipes 0) (List.nth recipes 1) in
  let scoreboard = MapList.push_seq (List.to_seq new_recipes) s.scoreboard in

  let new_elf elf_num recipe_num : int =
    let advance = MapList.nth s.scoreboard recipe_num + 1 in
    let new_recipe = (recipe_num + advance) mod MapList.length scoreboard in
    new_recipe
  in

  let elves = s.elves |> List.mapi new_elf in

  { elves; scoreboard }
;;

let rec step_forever s () = Seq.Cons (s, step_forever (step s))

let initial_state = { elves= [ 0; 1 ]; scoreboard= MapList.of_list [ 3; 7 ] }

let get_input ch : int = input_line ch |> String.trim |> int_of_string

let seq_viewer v = Seq.map (fun a -> v a ; a)

let seq_viewer_if c v s = if c then seq_viewer v s else s

(** An infinite sequence of ints that increments by one, starting at n *)
let rec seq_count n : int Seq.t = fun () -> Seq.Cons (n, seq_count (n + 1))

(** Zip sequences a and b into a sequence of tuples. Returns Nil if either a or b returns Nil. *)
let rec seq_join (a : 'a Seq.t) (b : 'b Seq.t) : ('a * 'b) Seq.t =
 fun () ->
  match a () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (a', a's) -> (
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (b', b's) -> Seq.Cons ((a', b'), seq_join a's b's) )
;;

let seq_enumerate s = seq_join (seq_count 0) s

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

let () =
  let max_recipes = get_input stdin in

  let state =
    step_forever initial_state
    |> seq_viewer_if verbose (output_state stderr)
    |> Seq.drop_while (fun s -> MapList.length s.scoreboard < max_recipes + 10)
    |> Seq.uncons
    |> Option.get
    |> fst
  in

  let next_ten =
    state.scoreboard
    |> MapList.to_seq
    |> Seq.drop max_recipes
    |> Seq.take 10
    |> List.of_seq
  in

  flush stderr ;

  next_ten |> List.map string_of_int |> String.concat "" |> output_string stdout ;

  output_char stdout '\n' ;

  assert (List.length next_ten = 10)
;;
