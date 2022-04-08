#! /usr/bin/env ocaml

(* Day 13 Part 2: Mine Cart Madness
 * 
 * USAGE:
 * 
 * ./day13p2.ml < input.txt
 * 
 * CONTEXT:
 *
 * There isn't much you can do to prevent crashes in this ridiculous system. 
 * However, by predicting the crashes, the Elves know where to be in advance 
 * and instantly remove the two crashing carts the moment any crash occurs.
 * 
 * They can proceed like this for a while, but eventually, they're going to run 
 * out of carts. It could be useful to figure out where the last cart that 
 * hasn't crashed will end up.
 * 
 * For example:
 * 
 *     />-<\  
 *     |   |  
 *     | /<+-\
 *     | | | v
 *     \>+</ |
 *       |   ^
 *       \<->/
 * 
 *     /---\  
 *     |   |  
 *     | v-+-\
 *     | | | |
 *     \-+-/ |
 *       |   |
 *       ^---^
 * 
 *     /---\  
 *     |   |  
 *     | /-+-\
 *     | v | |
 *     \-+-/ |
 *       ^   ^
 *       \---/
 * 
 *     /---\  
 *     |   |  
 *     | /-+-\
 *     | | | |
 *     \-+-/ ^
 *       |   |
 *       \---/
 * 
 * After four very expensive crashes, a tick ends with only one cart remaining; 
 * its final location is 6,4.
 * 
 * What is the location of the last cart at the end of the first tick where it 
 * is the only cart left?
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

let finger_map (f : 'b list -> 'a list -> 'b list * 'a list) (l : 'a list) :
    'b list =
  let rec finger_map' f visited unvisited =
    match unvisited with
    | [] -> visited
    | _ ->
        let visited, unvisited = f visited unvisited in
        finger_map' f visited unvisited
  in
  finger_map' f [] l
;;

let step ((paths, carts) : state) : state =
  let compare' = compare_map (fun (pos, _c) -> pos) Point.compare in
  let update_cart (moved : placed_cart list) (unmoved : placed_cart list) =
    match unmoved with
    | [] -> (moved, unmoved)
    | (pos, c) :: unmoved ->
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
              (Printf.sprintf "Cart (%d, %d) has gone off the tracks!" pos.x
                 pos.y )
        in

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

        (* check for collisions against moved carts AND carts yet to move *)
        let did_collide = ref false in
        let remove_colliding =
          List.filter (fun pc' ->
              let is_collision = compare' pc' (new_pos, new_cart) = 0 in
              if is_collision then did_collide := true else () ;
              not is_collision )
        in

        let moved = remove_colliding moved
        and unmoved = remove_colliding unmoved in

        let moved =
          if not !did_collide then (new_pos, new_cart) :: moved else moved
        in
        (moved, unmoved)
  in

  let carts = List.sort compare' carts in
  let new_carts = finger_map update_cart carts |> List.sort compare' in
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

let is_one_cart_left ((_paths, carts) : state) : bool = List.length carts <= 1

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
  let max_iters = 13000 in

  let state = get_input () |> parse_input in

  (* output_string stdout "done" *)
  let last_cart : point =
    Seq.zip (count_up_from 0) (step_seq state)
    (* |> seq_viewer (fun (i, s) -> *)
    (* Printf.eprintf "Step %d\n" i ; *)
    (* s |> viz_state |> output_string stderr ; *)
    (* flush stderr ) *)
    |> Seq.drop_while (fun (i, s) -> i <= max_iters && not (is_one_cart_left s))
    |> Seq.uncons
    |> Option.get
    |> fst
    |> snd
    |> snd
    |> function
    | [ (p, _cart) ] -> p
    | [] -> failwith "No carts left!"
    | _ -> failwith (Printf.sprintf "reached max iterations (%d)" max_iters)
  in
  Printf.printf "%d,%d\n" last_cart.x last_cart.y
;;
