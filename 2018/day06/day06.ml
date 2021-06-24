#! /usr/bin/env ocaml
(* Day 6: Chronal Coordinates
 *
 * Computing the largest finite area of a manhattan voronoi diagram
 *
 * USAGE:
 *
 *    ./day06.ml < input.txt
 *
 * CONTEXT:
 *
 * ...
 * If they're dangerous, maybe you can minimize the danger by finding the
 * coordinate that gives the largest distance from the other points.
 *
 * Using only the Manhattan distance, determine the area around each coordinate
 * by counting the number of integer X,Y locations that are closest to that
 * coordinate (and aren't tied in distance to any other coordinate).
 *
 * Your goal is to find the size of the largest area that isn't infinite. For
 * example, consider the following list of coordinates:
 *
 *     1, 1
 *     1, 6
 *     8, 3
 *     3, 4
 *     5, 5
 *     8, 9
 *
 * If we name these coordinates A through F, we can draw them on a grid,
 * putting 0,0 at the top left:
 *
 *     ..........
 *     .A........
 *     ..........
 *     ........C.
 *     ...D......
 *     .....E....
 *     .B........
 *     ..........
 *     ..........
 *     ........F.
 *
 * This view is partial - the actual grid extends infinitely in all
 * directions. Using the Manhattan distance, each location's closest coordinate
 * can be determined, shown here in lowercase:
 *
 *     aaaaa.cccc
 *     aAaaa.cccc
 *     aaaddecccc
 *     aadddeccCc
 *     ..dDdeeccc
 *     bb.deEeecc
 *     bBb.eeee..
 *     bbb.eeefff
 *     bbb.eeffff
 *     bbb.ffffFf
 *
 * Locations shown as . are equally far from two or more coordinates, and
 * so they don't count as being closest to any.
 *
 * In this example, the areas of coordinates A, B, C, and F are infinite -
 * while not shown here, their areas extend forever outside the visible
 * grid. However, the areas of coordinates D and E are finite: D is closest
 * to 9 locations, and E is closest to 17 (both including the coordinate's
 * location itself). Therefore, in this example, the size of the largest
 * area is 17.
 *
 * What is the size of the largest area that isn't infinite?
 *)

(* line reader *)
let line_seq_of_channel channel =
  let rec seq_of_stream stream =
    (fun () ->
      match Stream.peek stream with
      | Some _ -> Seq.Cons (Stream.next stream, seq_of_stream stream)
      | None -> Nil
    )
  in
  seq_of_stream
    (Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None))

type point = (int * int)

let parse_point (s: string) : (point, string) Result.t =
  s
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map int_of_string_opt
  |> function
    | Some x :: Some y :: [] -> Ok (x, y)
    | _ -> Error "Couldn't parse line"

module Point =
struct
  type t = point
  let compare (a: point) (b: point) : int =
    match a, b with
    | (x1, y1), (x2, y2) ->
      match Int.compare x1 x2 with
      | 0 -> Int.compare y1 y2
      | c -> c
  let taxi_dist a b =
    match a, b with | (x1, y1), (x2, y2) ->
    (Int.abs (x1 - x2)) + (Int.abs (y1 - y2))
end

module PointMap = Map.Make(Point)

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

module Grid =
struct
  type t = { w: int; h: int; x: int; y: int; _inner: string option array}
  let make x y w h = {
    w; h; x; y;
    _inner = Array.make (w * h) None;
    }

  let _is_in g x y =
    x >= g.x && x < g.x + g.w && y >= g.y && y < g.y + g.h

  let _xy2i g x y =
    (y - g.y) * g.w + (x - g.x)

  let get g x y : string option =
    if not (_is_in g x y) then Failure "out of bounds get" |> raise else
    Array.get g._inner (_xy2i g x y)

  let set g x y v =
    if not (_is_in g x y) then Failure "out of bounds set" |> raise else
    Array.set g._inner (_xy2i g x y) v


  (* all points (unordered) along the perimeter of g *)
  let perimeter_seq g : (int * int) Seq.t =
    let rec range s e : int Seq.t = fun () -> if s > e then Nil else Cons (s, range (s + 1) e) in
    (* top/bottom *)
    range g.x (g.x + g.w - 1)
    |> Seq.flat_map (fun x -> List.to_seq [(x, g.y); (x, g.y + g.h - 1)])
    (* sides (excluding top/bottom) *)
    |> Seq.append (
      (range (g.y + 1) (g.y + g.h - 2))
      |> Seq.flat_map (fun y -> List.to_seq [(g.x, y); (g.x + g.w - 1, y)])
    )

  let to_seq g = Array.to_seq g._inner
end

let parse_or_fail (parser: string -> ('a, string) Result.t) (stream: string Seq.t) : 'a Seq.t =
  let line_count = ref 0 in
  stream
  |> Seq.map (fun m ->
    line_count := !line_count + 1; m)
  |> Seq.map parser
  |> Seq.map (function
    | Ok c -> c
    | Error e ->
      Printf.sprintf "line %d: %s" !line_count e |> failwith)

let find_nearest (m: 'a PointMap.t) (p: point) : 'a Option.t =
  let dist_to = Point.taxi_dist p in
  m
  |> PointMap.to_seq
  |> Seq.map (function | (p, name) -> name, (dist_to p))
  |> List.of_seq
  |> List.sort (fun p1 p2 -> match p1, p2 with | (_, d1), (_, d2) -> Int.compare d1 d2)
  |> function
    | (_, d1)::(_, d2)::_ when d1 = d2 -> None
    | (n, _)::_ -> Some n
    | [] -> None

let bounds m =
    m
    |> PointMap.to_seq
    |> Seq.map (function | p, _ -> p)
    |> (fun s -> match s () with
      | Nil -> failwith "Empty sequence for bounds"
      | Cons ((x, y), s) ->
        Seq.fold_left
        (fun bounds p -> match bounds, p with | (min_x, max_x, min_y, max_y), (x, y) ->
          (
            min min_x x,
            max max_x x,
            min min_y y,
            max max_y y
          ))
        (x, x, y, y)
        s
    )

(* Sequence of unique point names (AA, AB, AC... BA, BB, BC...) *)
let point_names () : string Seq.t =
  let rec chars start fin : char Seq.t = fun () ->
    let incr_char = fun c -> c |> Char.code |> (+) 1 |> Char.chr in
    if start > fin then Nil else Cons (start, chars (incr_char start) fin)
  in
  let chars () = chars 'A' 'Z' in
  let chars_to_string cs =
    cs |> List.to_seq |> String.of_seq
  in
  chars ()
  |> Seq.flat_map (fun c1 ->
    Seq.map (fun c2 ->
      [c1; c2] |> chars_to_string) (chars ()))

let rec join_seq a b = fun () ->
  match a () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (a', a's) ->
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (b', b's) -> Seq.Cons ((a', b'), (join_seq a's b's))


(* Draw a similar diagram/map to the question (Warning: LARGE) *)
let viz () =
  let points =
    stdin
    |> line_seq_of_channel
    |> parse_or_fail parse_point
    |> fun ps -> join_seq ps (point_names())
    |> PointMap.of_seq
  in

  let min_x, max_x, min_y, max_y = bounds points in
  if min_x = 0 || max_x = 0 || min_y = 0 || max_y = 0 then Failure "bad bounds" |> raise else

  let pad = 20 in

  for y = min_y - pad to max_y + pad do
    for x = min_x - pad to max_x + pad do
      output_string stdout (
        match PointMap.find_opt (x, y) points with
        | Some s -> s
        | None ->
          match find_nearest points (x, y) with
          | Some n -> String.lowercase_ascii n
          | None -> ". "
      );
    done;
    output_char stdout '\n';
  done

(* naive - make map sufficiently large and record set of all points on boundaries *)

(* Output the size of the largest area that isn't infinite *)
let main () =
    let points =
    stdin
    |> line_seq_of_channel
    |> parse_or_fail parse_point
    |> fun ps -> join_seq ps (point_names())
    |> PointMap.of_seq
  in

  let min_x, max_x, min_y, max_y = bounds points in
  if min_x = 0 || max_x = 0 || min_y = 0 || max_y = 0 then failwith "bad bounds" else

  let pad = 0 in
  (* let pad = 20 in *)
  (* let pad = List.fold_left max 0 [max_x - min_x; max_y - min_y] in *)

  let grid = Grid.make (min_x - pad) (min_y - pad) (max_x - min_x + pad) (max_y - min_y + pad) in

  Printf.eprintf "[grid]\nx\ty\tw\th\n%d\t%d\t%d\t%d\n" grid.x grid.y grid.w grid.h;
  flush stderr;

  for y = grid.y to grid.y + grid.h - 1 do
    for x = grid.x to grid.x + grid.w - 1 do
      (* Printf.eprintf "(%d, %d)\n" x y; *)
      Grid.set grid x y (
        match PointMap.find_opt (x, y) points with
        | Some s -> Some s
        | None -> find_nearest points (x, y)
      )
    done;
  done;

  let infinite_areas =
  grid
  |> Grid.perimeter_seq
  |> Seq.filter_map (function | x, y -> Grid.get grid x y)
  |> StringSet.of_seq
  in

  let area_counts =
  grid
  |> Grid.to_seq
  |> Seq.filter_map (fun o -> o)
  |> Seq.fold_left (fun m s -> match StringMap.find_opt s m with
    | Some count -> StringMap.add s (count + 1) m
    | None -> StringMap.add s 1 m
    ) StringMap.empty
  in

  let largest_finite_area, count =
    area_counts
    |> StringMap.to_seq
    |> Seq.filter (function | s, count -> Option.is_none (StringSet.find_opt s infinite_areas))
    |> List.of_seq
    |> List.sort (fun a b -> -Int.compare (snd a) (snd b))
    |> List.hd
  in

  Printf.printf "%d\n" count

let () =
  main ()
  (* viz () *)
