#! /usr/bin/env ocaml
(* Day 13: Mine Cart Madness
 * 
 * USAGE:
 * 
 * ./day13.ml < input.txt
 * 
 * CONTEXT:
 *
 * A crop of this size requires significant logistics to transport produce,
 * soil, fertilizer, and so on. The Elves are very busy pushing things around
 * in carts on some kind of rudimentary system of tracks they've come up with.
 * 
 * Seeing as how cart-and-track systems don't appear in recorded history
 * for another 1000 years, the Elves seem to be making this up as they go
 * along. They haven't even figured out how to avoid collisions yet.
 * 
 * You map out the tracks (your puzzle input) and see where you can help.
 * 
 * Tracks consist of straight paths (| and -), curves (/ and \), and
 * intersections (+). Curves connect exactly two perpendicular pieces of
 * track; for example, this is a closed loop:
 * 
 *     /----\
 *     |    |
 *     |    |
 *     \----/
 * 
 * Intersections occur when two perpendicular paths cross. At an intersection,
 * a cart is capable of turning left, turning right, or continuing
 * straight. Here are two loops connected by two intersections:
 * 
 *     /-----\
 *     |     |
 *     |  /--+--\
 *     |  |  |  |
 *     \--+--/  |
 *        |     |
 *        \-----/
 * 
 * Several carts are also on the tracks. Carts always face either up (^),
 * down (v), left (<), or right (>). (On your initial map, the track under
 * each cart is a straight path matching the direction the cart is facing.)
 * 
 * Each time a cart has the option to turn (by arriving at any intersection),
 * it turns left the first time, goes straight the second time, turns right
 * the third time, and then repeats those directions starting again with
 * left the fourth time, straight the fifth time, and so on. This process is
 * independent of the particular intersection at which the cart has arrived -
 * that is, the cart has no per-intersection memory.
 * 
 * Carts all move at the same speed; they take turns moving a single step at
 * a time. They do this based on their current location: carts on the top row
 * move first (acting from left to right), then carts on the second row move
 * (again from left to right), then carts on the third row, and so on. Once
 * each cart has moved one step, the process repeats; each of these loops
 * is called a tick.
 * 
 * For example, suppose there are two carts on a straight track:
 * 
 *     |  |  |  |  |
 *     v  |  |  |  |
 *     |  v  v  |  |
 *     |  |  |  v  X
 *     |  |  ^  ^  |
 *     ^  ^  |  |  |
 *     |  |  |  |  |
 * 
 * First, the top cart moves. It is facing down (v), so it moves down one
 * square. Second, the bottom cart moves. It is facing up (^), so it moves
 * up one square. Because all carts have moved, the first tick ends. Then,
 * the process repeats, starting with the first cart. The first cart moves
 * down, then the second cart moves up - right into the first cart, colliding
 * with it! (The location of the crash is marked with an X.) This ends the
 * second and last tick.
 * 
 * Here is a longer example:
 * 
 *     /->-\        
 *     |   |  /----\
 *     | /-+--+-\  |
 *     | | |  | v  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /-->\        
 *     |   |  /----\
 *     | /-+--+-\  |
 *     | | |  | |  |
 *     \-+-/  \->--/
 *       \------/   
 * 
 *     /---v        
 *     |   |  /----\
 *     | /-+--+-\  |
 *     | | |  | |  |
 *     \-+-/  \-+>-/
 *       \------/   
 * 
 *     /---\        
 *     |   v  /----\
 *     | /-+--+-\  |
 *     | | |  | |  |
 *     \-+-/  \-+->/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----\
 *     | /->--+-\  |
 *     | | |  | |  |
 *     \-+-/  \-+--^
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----\
 *     | /-+>-+-\  |
 *     | | |  | |  ^
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----\
 *     | /-+->+-\  ^
 *     | | |  | |  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----<
 *     | /-+-->-\  |
 *     | | |  | |  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /---<\
 *     | /-+--+>\  |
 *     | | |  | |  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /--<-\
 *     | /-+--+-v  |
 *     | | |  | |  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /-<--\
 *     | /-+--+-\  |
 *     | | |  | v  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /<---\
 *     | /-+--+-\  |
 *     | | |  | |  |
 *     \-+-/  \-<--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  v----\
 *     | /-+--+-\  |
 *     | | |  | |  |
 *     \-+-/  \<+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----\
 *     | /-+--v-\  |
 *     | | |  | |  |
 *     \-+-/  ^-+--/
 *       \------/   
 * 
 *     /---\        
 *     |   |  /----\
 *     | /-+--+-\  |
 *     | | |  X |  |
 *     \-+-/  \-+--/
 *       \------/   
 * 
 * After following their respective paths for a while, the carts eventually
 * crash. To help prevent crashes, you'd like to know the location of the
 * first crash. Locations are given in X,Y coordinates, where the furthest
 * left column is X=0 and the furthest top row is Y=0:
 * 
 *                111
 *      0123456789012
 *     0/---\        
 *     1|   |  /----\
 *     2| /-+--+-\  |
 *     3| | |  X |  |
 *     4\-+-/  \-+--/
 *     5  \------/   
 * 
 * In this example, the location of the first crash is 7,3.
 *)

(** direction a cart may face *)
type direction = Up | Down | Left | Right

let turn_left = function
  | Up -> Left
  | Down -> Right
  | Left -> Down
  | Right -> Up
;;

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down
;;

(** direction a cart may choose to go at an intersection *)
type turn_choice = Left | Straight | Right

(** next turn choice to make *)
let cycle_turn_choice = function
  | Left -> Straight
  | Straight -> Right
  | Right -> Left
;;

let make_turn (dir : direction) (choice : turn_choice) : direction =
  match choice with
  | Straight -> dir
  | Left -> turn_left dir
  | Right -> turn_right dir
;;

type path = Vertical | Horizontal | LeftUp | LeftDown | Intersection

let char_of_path = function
  | Vertical -> '|'
  | Horizontal -> '-'
  | LeftUp -> '/'
  | LeftDown -> '\\'
  | Intersection -> '+'
;;

type cart = { dir: direction; next_turn: turn_choice }

let char_of_cart { dir } =
  match dir with
  | Up -> '^'
  | Down -> 'v'
  | Left -> '<'
  | Right -> '>'
;;

module Point = struct
  type t = { x: int; y: int }

  (** Sorts carts by y, then x value *)
  let compare p1 p2 =
    match Int.compare p1.y p2.y with
    | 0 -> Int.compare p1.x p2.x
    | v -> v
  ;;
end

type point = Point.t

module PointMap = Map.Make (Point)

type placed_cart = point * cart

let rec char_seq_of_channel ch () =
  try Seq.Cons (input_char ch, char_seq_of_channel ch)
  with End_of_file -> Seq.Nil
;;

let rec pos_seq ?(p : point = { x= 0; y= 0 }) (cs : char Seq.t) :
    (char * point) Seq.t =
 fun () ->
  match cs () with
  | Nil -> Nil
  | Cons (c, cs) ->
      Cons
        ( (c, p)
        , pos_seq cs
            ~p:
              ( match c with
              | '\n' -> { x= 0; y= p.y + 1 }
              | _ -> { x= p.x + 1; y= p.y } ) )
;;

type state = path PointMap.t * placed_cart list

(**/**)

type item = Path of path | Cart of cart

(**/**)

let parse_input (ch : in_channel) : state =
  let parse_char (c, p) : (item * point) Seq.t =
    let one a = Seq.return (a, p)
    and two a b = Seq.cons (a, p) (Seq.return (b, p))
    and none = Seq.empty
    and make_cart d = { dir= d; next_turn= Left } in

    match c with
    (* paths *)
    | '|' -> one (Path Vertical)
    | '-' -> one (Path Horizontal)
    | '/' -> one (Path LeftUp)
    | '\\' -> one (Path LeftDown)
    | '+' -> one (Path Intersection)
    (* carts (also paths) *)
    | '^' -> two (Path Vertical) (Cart (make_cart Up))
    | 'v' -> two (Path Vertical) (Cart (make_cart Down))
    | '<' -> two (Path Horizontal) (Cart (make_cart Left))
    | '>' -> two (Path Horizontal) (Cart (make_cart Right))
    (* misc *)
    | ' ' | '\n' -> none
    | _ ->
        invalid_arg
          (Printf.sprintf "Line %d:%d: Unexpected char '%c'" (p.y + 1) (p.x + 1)
             c )
  and collect =
    Seq.fold_left
      (fun (paths, carts) (i, pos) ->
        match i with
        | Path p -> (PointMap.add pos p paths, carts)
        | Cart c -> (paths, List.cons (pos, c) carts) )
      (PointMap.empty, [])
  in
  ch |> char_seq_of_channel |> pos_seq |> Seq.flat_map parse_char |> collect
;;

let compare_map m (c : 'a -> 'a -> int) a b = c (m a) (m b)

(* map a list, with viewing mapped and unmapped element along the way *)
let ripple (f : 'b list -> 'a list -> 'a -> 'b) (l : 'a list) : 'b list =
  let rec ripple' f visited unvisited =
    match unvisited with
    | [] -> []
    | a :: rest ->
        let b = f visited rest a in
        b :: ripple' f (b :: visited) rest
  in
  ripple' f [] l
;;

let step ((paths, carts) : state) : state =
  let compare' = compare_map (fun (pos, _c) -> pos) Point.compare in
  let did_collide = ref false in
  let update_cart moved unmoved ((pos, c) : placed_cart) : placed_cart =
    (* stop processing if there is a collision *)
    if !did_collide then (pos, c)
    else
      let new_pos : point =
        (* coordinate system is
         * 0-->+
         * |
         * v
         * +
         *)
        match c.dir with
        | Up -> { x= pos.x; y= pos.y - 1 }
        | Down -> { x= pos.x; y= pos.y + 1 }
        | Left -> { x= pos.x - 1; y= pos.y }
        | Right -> { x= pos.x + 1; y= pos.y }
      in
      let new_path =
        try PointMap.find new_pos paths
        with Not_found ->
          (* either the simulation is buggy or the map is bad... *)
          failwith
            (Printf.sprintf "Cart (%d, %d) has gone off the tracks!" pos.x pos.y)
      in

      (* check for collisions against moved carts AND carts yet to move *)
      let is_collision =
        List.exists (fun pc' -> compare' pc' (new_pos, c) = 0)
      in
      if is_collision moved || is_collision unmoved then did_collide := true
      else () ;

      (* update direction *)
      let new_cart : cart =
        match (new_path, c) with
        (* already turned for straight paths *)
        | (Vertical | Horizontal), _ -> c
        (* update direction if entered turn *)
        (* LeftUp turn
         *   |
         * --/--
         *   |
         *)
        | LeftUp, { dir= Left; next_turn } -> { dir= Down; next_turn }
        | LeftUp, { dir= Down; next_turn } -> { dir= Left; next_turn }
        | LeftUp, { dir= Right; next_turn } -> { dir= Up; next_turn }
        | LeftUp, { dir= Up; next_turn } -> { dir= Right; next_turn }
        (* LeftDown turn
         *   |
         * --\--
         *   |
         *)
        | LeftDown, { dir= Left; next_turn } -> { dir= Up; next_turn }
        | LeftDown, { dir= Up; next_turn } -> { dir= Left; next_turn }
        | LeftDown, { dir= Right; next_turn } -> { dir= Down; next_turn }
        | LeftDown, { dir= Down; next_turn } -> { dir= Right; next_turn }
        (* turn at intersection  *)
        | Intersection, { dir; next_turn } ->
            { dir= make_turn dir next_turn
            ; next_turn= cycle_turn_choice next_turn
            }
      in
      (new_pos, new_cart)
  in

  let carts = List.sort compare' carts in
  let new_carts = ripple update_cart carts |> List.sort compare' in
  (paths, new_carts)
;;

let count_up_from = Seq.iterate succ

let step_seq = Seq.iterate step

let get_crashes ((_paths, carts) : state) : (point * cart) list Seq.t =
  let compare' = compare_map (fun (pos, _c) -> pos) Point.compare in

  let rec group_by ?(grp = []) cmp (s : 'a Seq.t) : 'a list Seq.t =
    match (grp, s ()) with
    | [], Nil -> Seq.empty
    | _ :: _, Nil -> Seq.return grp
    | [], Cons (g, s') -> group_by ~grp:[ g ] cmp s'
    | g :: _, Cons (maybe_g, s') ->
        if cmp g maybe_g = 0 then group_by ~grp:(maybe_g :: grp) cmp s'
        else Seq.cons grp (group_by cmp s')
  in

  carts
  |> List.sort compare'
  |> List.to_seq
  |> group_by compare'
  |> Seq.filter (fun l -> List.length l > 1)
;;

let get_crashes_pos (s : state) : point Seq.t =
  get_crashes s |> Seq.map (fun carts -> carts |> List.hd |> fst)
;;

let is_crash (s : state) : bool = get_crashes s |> Seq.is_empty |> not

let viz_state (paths, carts) : string =
  let max_x, max_y =
    paths
    |> PointMap.to_seq
    |> Seq.fold_left
         (fun (mx, my) ({ Point.x; y }, _) -> (max mx x, max my y))
         (0, 0)
  in
  let width = max_x + 2 (* include \n at end of each row *)
  and height = max_y + 1 in
  let buffer = Bytes.make (width * height) ' ' in
  let set { Point.x; y } = Bytes.set buffer ((width * y) + x) in

  (* line endings *)
  for y = 0 to max_y do
    set { x= max_x + 1; y } '\n'
  done ;

  (* paths *)
  PointMap.iter (fun pos path -> set pos (char_of_path path)) paths ;

  (* carts *)
  List.iter (fun (pos, cart) -> set pos (char_of_cart cart)) carts ;

  (* crashes *)
  get_crashes_pos (paths, carts) |> Seq.iter (fun pos -> set pos 'X') ;

  Bytes.to_string buffer
;;

(** Try to open the first argument or stdin *)
let get_input () = try open_in Sys.argv.(1) with Invalid_argument _ -> stdin

(** Read file as line iterator *)
let rec line_seq_of_channel ch () =
  try Seq.Cons (input_line ch, line_seq_of_channel ch)
  with End_of_file -> Seq.Nil
;;

let seq_viewer v = Seq.map (fun a -> v a ; a)

let () =
  let max_iters = 1000 in

  let state = get_input () |> parse_input in

  (* output_string stdout "done" *)
  let crash : point =
    Seq.zip (count_up_from 0) (step_seq state)
    (* |> seq_viewer (fun (i, s) -> *)
           (* Printf.eprintf "Step %d\n" i ; *)
           (* s |> viz_state |> output_string stderr ; *)
           (* flush stderr ) *)
    |> Seq.drop_while (fun (i, s) -> i <= max_iters && not (is_crash s))
    |> Seq.uncons
    |> Option.get
    |> fst
    |> snd
    |> get_crashes_pos
    |> Seq.uncons
    |> function
    | None -> failwith (Printf.sprintf "reached max iterations (%d)" max_iters)
    | Some (p, _seq) -> p
  in
  Printf.printf "%d,%d\n" crash.x crash.y
;;
