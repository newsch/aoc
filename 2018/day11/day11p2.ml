#! /usr/bin/env ocaml
(* Day 11: Chronal Charge
 *
 * Find the square area of any dimension of a 300x300 grid with the
 * greatest sum.
 *
 * Using a Summed Area Table reduced runtime with ocamlopt from 40 minutes to
 * 600 milliseconds, see <https://en.wikipedia.org/wiki/Summed-area_table>
 * 
 * USAGE:
 *
 *     ./day11p2.ml < input.txt
 *
 * CONTEXT:
 *
 * You watch the Elves and their sleigh fade into the distance as they head
 * toward the North Pole.
 * 
 * Actually, you're the one fading. The falling sensation returns.
 * 
 * The low fuel warning light is illuminated on your wrist-mounted
 * device. Tapping it once causes it to project a hologram of the situation:
 * a 300x300 grid of fuel cells and their current power levels, some
 * negative. You're not sure what negative power means in the context of
 * time travel, but it can't be good.
 * 
 * Each fuel cell has a coordinate ranging from 1 to 300 in both the X
 * (horizontal) and Y (vertical) direction. In X,Y notation, the top-left
 * cell is 1,1, and the top-right cell is 300,1.
 * 
 * The interface lets you select any 3x3 square of fuel cells. To increase
 * your chances of getting to your destination, you decide to choose the
 * 3x3 square with the largest total power.
 * 
 * The power level in a given fuel cell can be found through the following
 * process:
 * 
 *     Find the fuel cell's rack ID, which is its X coordinate plus 10.
 *     Begin with a power level of the rack ID times the Y coordinate.
 *     Increase the power level by the value of the grid serial number
 *     (your puzzle input).  Set the power level to itself multiplied by the
 *     rack ID.  Keep only the hundreds digit of the power level (so 12345
 *     becomes 3; numbers with no hundreds digit become 0).  Subtract 5 from
 *     the power level.
 * 
 * For example, to find the power level of the fuel cell at 3,5 in a grid
 * with serial number 8:
 * 
 *     The rack ID is 3 + 10 = 13.  The power level starts at 13 * 5 = 65.
 *     Adding the serial number produces 65 + 8 = 73.  Multiplying by the
 *     rack ID produces 73 * 13 = 949.  The hundreds digit of 949 is 9.
 *     Subtracting 5 produces 9 - 5 = 4.
 * 
 * So, the power level of this fuel cell is 4.
 * 
 * Here are some more example power levels:
 * 
 *     Fuel cell at  122,79, grid serial number 57: power level -5.
 *     Fuel cell at 217,196, grid serial number 39: power level  0.
 *     Fuel cell at 101,153, grid serial number 71: power level  4.
 * 
 * Your goal is to find the 3x3 square which has the largest total power. The
 * square must be entirely within the 300x300 grid. Identify this square
 * using the X,Y coordinate of its top-left fuel cell. For example:
 * 
 * For grid serial number 18, the largest total 3x3 square has a top-left
 * corner of 33,45 (with a total power of 29); these fuel cells appear in
 * the middle of this 5x5 region:
 * 
 *     -2  -4   4   4   4
 *     -4   4   4   4  -5
 *      4   3   3   4  -4
 *      1   1   2   4  -3
 *     -1   0   2  -5  -2
 * 
 * For grid serial number 42, the largest 3x3 square's top-left is 21,61
 * (with a total power of 30); they are in the middle of this region:
 * 
 *     -3   4   2   2   2
 *     -4   4   3   3   4
 *     -5   3   3   4  -4
 *      4   3   3   4  -3
 *      3   3   3  -5  -1
 * 
 * What is the X,Y coordinate of the top-left fuel cell of the 3x3 square
 * with the largest total power?
 *)

(** Two-dimensional grid of power, 1-indexed *)
module Grid = struct
  type t = {
    a: int Array.t;
    w: int;
    h: int;
  }

  let xy2i g x y : int =
    (y - 1) * g.w + (x - 1)
  ;;

  let i2xy {w;} i : int * int =
    let x = (i mod w) + 1 in
    let y = (i / w) + 1 in
    x, y
  ;;

  let make w h serial_number =
    let extract_hundreds x = (x / 100) mod 10 in

    let calc_power (x, y) =
      let rack_id = x + 10 in

      (     
        extract_hundreds (
          (rack_id * y + serial_number) * rack_id
        )
      )
      - 5
    in

    let a = Array.make (w*h) 0 in

    Array.to_seqi a
    |> Seq.map (function | i, _v -> i, calc_power (i2xy {a; w; h;} i))
    |> Seq.iter (function | i, v -> Array.set a i v)
    ;

    {a; w; h;}
  ;;

  let to_seqxy g =
    Array.to_seqi g.a
    |> Seq.map (function | i, v -> (i2xy g i), v)
  ;;

  let get g x y =
    let i = xy2i g x y in
    (* assert (i < Array.length g.a && i >= 0); *)
    Array.get g.a i
  ;;

  let output ch g =
    for i = 0 to g.h - 1 do
      Array.sub g.a (i * g.w) g.w
      |> Array.to_seq
      |> Seq.iter (Printf.fprintf ch "%3d")
      ;
      output_char ch '\n';
    done
  ;;
end

(** Summed Area Table, 1-indexed *)
module SummedAreaTable = struct
  type t = {
    a: int Array.t;
    w: int;
    h: int;
  }

  (**/**)

  let is_in {a;} i : bool =
    i >= 0 &&
    i < Array.length a
  ;;

  let xy2i {w;} x y : int =
    (y - 1) * w + (x - 1)
  ;;

  let i2xy {w;} i : int * int =
    let x = (i mod w) + 1 in
    let y = (i / w) + 1 in
    x, y
  ;;

  (**/**)

  (** Returns the sum of all values ('x, 'y) where 'x <= x and 'y <= y *)
  let try_get sat x y : int option =
    if x < 0 || y < 0 then None else
    let i = xy2i sat x y in
    if is_in sat i
    then
      Some (Array.get sat.a i)
    else
      None
  ;;

  (** Compute the sum of an sub-region.

      Uses only 4 lookups regardless of sub-region/table size. *)
  let get_sum sat x y w h : int =
    let x1, y1 = x - 1, y - 1
    and x2, y2 = x + w - 1, y + h - 1
    and get x y : int = Option.value (try_get sat x y) ~default:0
    in

    (get x2 y2)
    + (get x1 y1)
    - (get x2 y1)
    - (get x1 y2)
  ;;

  (** Compute the SummedAreaTable of a Grid *)
  let make (g: Grid.t) : t =
    let sat = {
      a = Array.make (g.w*g.h) 0;
      w = g.w;
      h = g.h;
    } in

    let value x y : int = Option.value (try_get sat x y) ~default:0 in

    for y = 1 to sat.h do
      for x = 1 to sat.w do
        (Grid.get g x y)
        + (value x (y - 1))
        + (value (x - 1) y)
        - (value (x - 1) (y - 1))
        |> Array.set sat.a (xy2i sat x y)
      done
    done;

    sat
  ;;

  let to_seqxy sat =
    Array.to_seqi sat.a
    |> Seq.map (function | i, v -> (i2xy sat i), v)
  ;;

end

(** inclusive sequence of start -> stop *)
let rec range start stop: int Seq.t = fun () ->
  if start > stop
  then Seq.Nil
  else Seq.Cons (start, (range (start + 1) stop))
;;

(** Zip sequences a and b into a sequence of tuples.

    Returns Nil if either a or b returns Nil. *)
let rec seq_join (a: 'a Seq.t) (b: 'b Seq.t) : ('a * 'b) Seq.t = fun () ->
  match a () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (a', a's) ->
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (b', b's) -> Seq.Cons ((a', b'), (seq_join a's b's))
;;

let rec seq_starmap a b =
  Seq.flat_map (fun a' -> Seq.map (fun b' -> a', b') b) a
;;

let seq_max (cmp: 'a -> 'a -> int) (s: 'a Seq.t) : 'a =
  let max_fold a b =
    if (cmp a b > 0)
    then a
    else b
  in 
  match s () with
  | Nil -> failwith "empty sequence"
  | Cons (a, s') -> Seq.fold_left max_fold a s'
;;

let seq_viewer v = Seq.map (fun a -> v a; a);;

(** Apply m to a and b, then pass to c *)
let compare_map m c a b = c (m a) (m b);;

let sum_subgrids d (g: SummedAreaTable.t) =
  let subgrids : (int * int) Seq.t =
    range 1 (g.h - d) (* y *)
    |> Seq.flat_map (fun y ->
      range 1 (g.w - d) (* x *)
      |> Seq.map (fun x -> x,y))
  in
  subgrids
  |> Seq.map (fun (x, y) -> (x, y), SummedAreaTable.get_sum g x y d d)
;;

let () =
  (* read serial number from file *)
  let serial_number = stdin |> input_line |> int_of_string in

  (* calculate power of full grid *)
  let fuel_cells = Grid.make 300 300 serial_number in

  (* Grid.output stderr fuel_cells; flush stderr; *)

  let sat = SummedAreaTable.make fuel_cells in

  (* calculate power of each set of 1x1 to 300x300 *)
  range 1 300
  (* |> seq_viewer (fun i -> Printf.fprintf stderr "Start %d %f\n" i (Sys.time ()); flush stderr;) *)
  |> Seq.flat_map (fun d -> sum_subgrids d sat |> Seq.map (fun ((x, y), t) -> (x, y, d), t))
  |> seq_max (compare_map (fun ((_x, _y, _d), t) -> t) Int.compare)
  |> (fun ((x, y, d), t) -> Printf.printf "%dx%d (%d, %d): %d\n" d d x y t)
  ;
;;
