#! /usr/bin/env ocaml
(* Day 10: The Stars Align
 * 
 * CONTEXT:
 *
 * It's no use; your navigation system simply isn't capable of providing
 * walking directions in the arctic circle, and certainly not in 1018.
 * 
 * The Elves suggest an alternative. In times like these, North Pole rescue
 * operations will arrange points of light in the sky to guide missing Elves
 * back to base. Unfortunately, the message is easy to miss: the points move
 * slowly enough that it takes hours to align them, but have so much momentum
 * that they only stay aligned for a second. If you blink at the wrong time,
 * it might be hours before another message appears.
 * 
 * You can see these points of light floating in the distance, and record
 * their position in the sky and their velocity, the relative change in
 * position per second (your puzzle input). The coordinates are all given
 * from your perspective; given enough time, those positions and velocities
 * will move the points into a cohesive message!
 * 
 * Rather than wait, you decide to fast-forward the process and calculate
 * what the points will eventually spell.
 * 
 * For example, suppose you note the following points:
 * 
 *     position=< 9,  1> velocity=< 0,  2>
 *     position=< 7,  0> velocity=<-1,  0>
 *     position=< 3, -2> velocity=<-1,  1>
 *     position=< 6, 10> velocity=<-2, -1>
 *     position=< 2, -4> velocity=< 2,  2>
 *     position=<-6, 10> velocity=< 2, -2>
 *     position=< 1,  8> velocity=< 1, -1>
 *     position=< 1,  7> velocity=< 1,  0>
 *     position=<-3, 11> velocity=< 1, -2>
 *     position=< 7,  6> velocity=<-1, -1>
 *     position=<-2,  3> velocity=< 1,  0>
 *     position=<-4,  3> velocity=< 2,  0>
 *     position=<10, -3> velocity=<-1,  1>
 *     position=< 5, 11> velocity=< 1, -2>
 *     position=< 4,  7> velocity=< 0, -1>
 *     position=< 8, -2> velocity=< 0,  1>
 *     position=<15,  0> velocity=<-2,  0>
 *     position=< 1,  6> velocity=< 1,  0>
 *     position=< 8,  9> velocity=< 0, -1>
 *     position=< 3,  3> velocity=<-1,  1>
 *     position=< 0,  5> velocity=< 0, -1>
 *     position=<-2,  2> velocity=< 2,  0>
 *     position=< 5, -2> velocity=< 1,  2>
 *     position=< 1,  4> velocity=< 2,  1>
 *     position=<-2,  7> velocity=< 2, -2>
 *     position=< 3,  6> velocity=<-1, -1>
 *     position=< 5,  0> velocity=< 1,  0>
 *     position=<-6,  0> velocity=< 2,  0>
 *     position=< 5,  9> velocity=< 1, -2>
 *     position=<14,  7> velocity=<-2,  0>
 *     position=<-3,  6> velocity=< 2, -1>
 * 
 * Each line represents one point. Positions are given as <X, Y> pairs:
 * X represents how far left (negative) or right (positive) the point
 * appears, while Y represents how far up (negative) or down (positive)
 * the point appears.
 * 
 * At 0 seconds, each point has the position given. Each second, each point's
 * velocity is added to its position. So, a point with velocity <1, -2>
 * is moving to the right, but is moving upward twice as quickly. If this
 * point's initial position were <3, 9>, after 3 seconds, its position would
 * become <6, 3>.
 * 
 * Over time, the points listed above would move like this:
 * 
 * Initially:
 *     ........#.............
 *     ................#.....
 *     .........#.#..#.......
 *     ......................
 *     #..........#.#.......#
 *     ...............#......
 *     ....#.................
 *     ..#.#....#............
 *     .......#..............
 *     ......#...............
 *     ...#...#.#...#........
 *     ....#..#..#.........#.
 *     .......#..............
 *     ...........#..#.......
 *     #...........#.........
 *     ...#.......#..........
 * 
 * After 1 second:
 *     ......................
 *     ......................
 *     ..........#....#......
 *     ........#.....#.......
 *     ..#.........#......#..
 *     ......................
 *     ......#...............
 *     ....##.........#......
 *     ......#.#.............
 *     .....##.##..#.........
 *     ........#.#...........
 *     ........#...#.....#...
 *     ..#...........#.......
 *     ....#.....#.#.........
 *     ......................
 *     ......................
 * 
 * After 2 seconds:
 *     ......................
 *     ......................
 *     ......................
 *     ..............#.......
 *     ....#..#...####..#....
 *     ......................
 *     ........#....#........
 *     ......#.#.............
 *     .......#...#..........
 *     .......#..#..#.#......
 *     ....#....#.#..........
 *     .....#...#...##.#.....
 *     ........#.............
 *     ......................
 *     ......................
 *     ......................
 * 
 * After 3 seconds:
 *     ......................
 *     ......................
 *     ......................
 *     ......................
 *     ......#...#..###......
 *     ......#...#...#.......
 *     ......#...#...#.......
 *     ......#####...#.......
 *     ......#...#...#.......
 *     ......#...#...#.......
 *     ......#...#...#.......
 *     ......#...#..###......
 *     ......................
 *     ......................
 *     ......................
 *     ......................
 * 
 * After 4 seconds:
 *     ......................
 *     ......................
 *     ......................
 *     ............#.........
 *     ........##...#.#......
 *     ......#.....#..#......
 *     .....#..##.##.#.......
 *     .......##.#....#......
 *     ...........#....#.....
 *     ..............#.......
 *     ....#......#...#......
 *     .....#.....##.........
 *     ...............#......
 *     ...............#......
 *     ......................
 *     ......................
 * 
 * After 3 seconds, the message appeared briefly: HI. Of course, your message
 * will be much longer and will take many more seconds to appear.
 * 
 * What message will eventually appear in the sky?
 *)

let line_seq_of_channel channel =
  let rec seq_of_stream (stream : 'a Stream.t) : 'a Seq.t = fun () ->
    match Stream.peek stream with
    | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
    | None -> Nil
  in
  seq_of_stream
    (Stream.from (fun _ ->
         try Some (input_line channel) with End_of_file -> None))
;;

let seq_viewer v = Seq.map (fun a -> v a; a);;

(** An infinite sequence of ints that increments by one, starting at n *)
let rec seq_count n : int Seq.t = fun () ->
  Seq.Cons (n, (seq_count (n + 1)))
;;

(** Zip sequences a and b into a sequence of tuples. Returns Nil if either a or b returns Nil. *)
let rec seq_join (a: 'a Seq.t) (b: 'b Seq.t) : ('a * 'b) Seq.t = fun () ->
  match a () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (a', a's) ->
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (b', b's) -> Seq.Cons ((a', b'), (seq_join a's b's))
;;

let seq_enumerate s = seq_join (seq_count 0) s;;

(** Return Nil once an item in s does not satisfy predicate p *)
let rec seq_abort p s = fun () ->
  match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (a, s') -> if p a then Seq.Cons (a, (seq_abort p s')) else Seq.Nil
;;

(** Return the n "lowest" items in sequence s, as determined by cmp *)
let seq_min_n (n: int) (cmp: 'a -> 'a -> int) (s: 'a Seq.t) : 'a list =
  let min_n_fold l i =
    l
    |> List.cons i
    |> List.sort cmp
    |> List.filteri (fun i _ -> i < n)
  in 
  Seq.fold_left min_n_fold [] s
;;

let compare_map m c a b = c (m a) (m b);;

module Point = struct
  type t = {
    x: int;
    y: int;
  }
end

module Light = struct
  type t = {
    pos: Point.t;
    delta: Point.t;
  }

  (* "position=<-42601, -53357> velocity=< 4,  5>" *)
  let parse (s: string) : t =
    let parse_pair s = s
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.map int_of_string
      |> function
        | [a; b] -> (a, b)
        | _ -> failwith "Bad number of args"
    in

    s
    |> String.split_on_char '<'
    |> List.map (String.split_on_char '>')
    |> List.flatten
    |> (function
      | [ "position="; position; " velocity="; velocity; _line_end ] -> ((parse_pair position), (parse_pair velocity))
      | _ -> failwith "Bad input")
    |> (function
      | ((x, y), (dx, dy)) -> { pos={ x; y; }; delta={ x=dx; y=dy; }})
  ;;

  let output ch l =
    Printf.fprintf ch "(%d, %d)\t[%d, %d]\n" l.pos.x l.pos.y l.delta.x l.delta.y
  ;;

  let step { pos={ x; y; }; delta; } =
    let x = x + delta.x in
    let y = y + delta.y in
    {pos={x; y;}; delta; }
  ;;

  let rec step_forever ls = fun () ->
    Seq.Cons (ls, (step_forever (List.map step ls)))
  ;;

end

type box = {
  min_x: int;
  max_x: int;
  min_y: int;
  max_y: int;
}

let bounds (s: Light.t Seq.t) : box option =
  let bounds_fold (bounds: box option) ({ x; y; }: Point.t) =
    match bounds with
    | None -> Some { min_x=x; max_x=x; min_y=y; max_y=y; }
    | Some { min_x; max_x; min_y; max_y; } ->
      let min_x = min min_x x in
      let max_x = max max_x x in
      let min_y = min min_y y in
      let max_y = max max_y y in
      Some { min_x; max_x; min_y; max_y; }
  in
  s
  |> Seq.map (fun (l: Light.t) -> l.pos)
  |> Seq.fold_left bounds_fold None
;;

let bounds_area b = (b.max_x - b.min_x + 1) * (b.max_y - b.min_y + 1);;

let output_sky ch (ls: Light.t List.t) =
  let b = ls |> List.to_seq |> bounds |> Option.get in
  let sky = Array.make (bounds_area b) '.' in
  let sky_width = b.max_x - b.min_x + 1 in
  let sky_height = b.max_y - b.min_y  + 1 in

  Printf.eprintf "array size: %d\n" (Array.length sky);
  Printf.eprintf "sky width: %d\n" sky_width;
  Printf.eprintf "sky height: %d\n" sky_height;

  let convert (l: Light.t) : int = (l.pos.y - b.min_y) * sky_width + (l.pos.x - b.min_x) in

  List.iter (fun (l: Light.t) -> (* Printf.eprintf "(%d, %d): %d\n" l.pos.x l.pos.y (convert l); flush stderr; *) Array.set sky (convert l) '#') ls;
  
  for i = 0 to sky_height - 1 do
    let row = Array.sub sky (i * sky_width) sky_width in
    row 
    |> Array.to_seq 
    |> fun s -> Seq.append s (Seq.return '\n') 
    |> String.of_seq 
    |> output_string ch
    ;
    flush ch;
  done
;;

let () =
  let max_iter = 15000 in

  (* parse lights *)
  stdin
  |> line_seq_of_channel
  |> Seq.map Light.parse
  (* |> seq_viewer (Light.output stderr) *)
  |> List.of_seq
  |> (fun ls ->
    Printf.fprintf stderr "%d lights " (List.length ls);
    flush stderr;
    let bounds = List.to_seq ls |> bounds |> Option.get in
    Printf.fprintf stderr "%d; %d x %d; (%d, %d) -> (%d, %d)\n" (bounds_area bounds) (bounds.max_x - bounds.min_x) (bounds.max_y - bounds.min_y) bounds.min_x bounds.min_y bounds.max_x bounds.max_y;
    flush stderr;
    ls)
  (* step lights forward *)
  |> Light.step_forever
  (* stop after # of iterations *)
  |> seq_enumerate
  |> seq_abort (function | i, _ls -> i < max_iter)
  (* calculate bounds *)
  |> Seq.map (function | i, ls -> let b = (List.to_seq ls) |> bounds |> Option.get in i, ls, b, (bounds_area b))
  (* save times with smallest bounds *)
  |> seq_min_n 1 (compare_map (function | _i, _ls, _bounds, area -> area) Int.compare)
  (* print lights *)
  |> List.iter (function | i, ls, bounds, area ->
    Printf.printf "%d: %d; %d x %d; (%d, %d) -> (%d, %d)\n" i area (bounds.max_x - bounds.min_x) (bounds.max_y - bounds.min_y) bounds.min_x bounds.min_y bounds.max_x bounds.max_y;
    flush stdout;
    output_sky stdout ls;
    flush stdout;
    )
;;
