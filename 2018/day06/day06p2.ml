#! /usr/bin/env ocaml
(* Day 6 Part 2: Chronal Coordinates
 *
 * USAGE:
 *
 *    ./day06.ml < input.txt
 *
 * CONTEXT:
 *
 * ...
 * On the other hand, if the coordinates are safe, maybe the best you can
 * do is try to find a region near as many coordinates as possible.
 *
 * For example, suppose you want the sum of the Manhattan distance to all
 * of the coordinates to be less than 32. For each location, add up the
 * distances to all of the given coordinates; if the total of those distances
 * is less than 32, that location is within the desired region. Using the
 * same coordinates as above, the resulting region looks like this:
 *
 *     ..........
 *     .A........
 *     ..........
 *     ...###..C.
 *     ..#D###...
 *     ..###E#...
 *     .B.###....
 *     ..........
 *     ..........
 *     ........F.
 *
 * In particular, consider the highlighted location 4,3 located at the top
 * middle of the region. Its calculation is as follows, where abs() is the
 * absolute value function:
 *
 * - Distance to coordinate A: abs(4-1) + abs(3-1) =  5
 * - Distance to coordinate B: abs(4-1) + abs(3-6) =  6
 * - Distance to coordinate C: abs(4-8) + abs(3-3) =  4
 * - Distance to coordinate D: abs(4-3) + abs(3-4) =  2
 * - Distance to coordinate E: abs(4-5) + abs(3-5) =  3
 * - Distance to coordinate F: abs(4-8) + abs(3-9) = 10
 * - Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
 *
 * Because the total distance to all coordinates (30) is less than 32,
 * the location is within the region.
 *
 * This region, which also includes coordinates D and E, has a total size
 * of 16.
 *
 * Your actual region will need to be much larger than this example, though,
 * instead including all locations with a total distance of less than 10000.
 *
 * What is the size of the region containing all locations which have a
 * total distance to all given coordinates of less than 10000?
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

let bounds m =
    m
    |> List.to_seq
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

let rec range s e : int Seq.t = fun () ->
  if s > e then Nil else Cons (s, (range (s + 1) e))

let grid_seq min_x max_x min_y max_y : (int * int) Seq.t =
  range min_y max_y
  |> Seq.flat_map (fun y -> (range min_x max_x) |> Seq.map (fun x -> (x, y)))

let () =
  let points =
    stdin
    |> line_seq_of_channel
    |> parse_or_fail parse_point
    |> List.of_seq
  in

  let min_x, max_x, min_y, max_y = bounds points in
  if min_x = 0 || max_x = 0 || min_y = 0 || max_y = 0 then Failure "bad bounds" |> raise else

  let dist_sums p : int =
    points
    |> List.to_seq
    |> Seq.map (Point.taxi_dist p)
    |> Seq.fold_left (+) 0
  in

  let num_close =
    grid_seq min_x max_x min_y max_y
    |> Seq.map dist_sums
    |> Seq.filter ((>) 10000)
    |> Seq.fold_left (fun acc _ -> acc + 1) 0
  in

  Printf.printf "%d\n" num_close
