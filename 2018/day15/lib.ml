module Point = struct
  type t = { x: int; y: int }

  (** compare for point following reading order (left to right, top to bottom) *)
  let compare p1 p2 =
    match (Int.compare p1.x p2.x, Int.compare p1.y p2.y) with
    | 0, 0 -> 0
    | x, 0 -> x
    | _, y -> y
  ;;
end

type unit = { hp: int; ap: int }

let default_unit = { hp= 200; ap= 3 }

module PointSet = Set.Make (Point)
module PointMap = Map.Make (Point)

type board = { height: int; width: int; walls: PointSet.t }

type state = { board: board; elves: unit PointMap.t; goblins: unit PointMap.t }

module StrBoard = struct
  (* array matrix of y, x *)
  type t = char array array

  let set b (p : Point.t) ch = b.(p.y).(p.x) <- ch

  let make w h : t = Array.make_matrix h w '.'

  let of_board b =
    let a = make b.width b.height in
    let set_wall p = set a p '#' in
    PointSet.iter set_wall b.walls ;
    a
  ;;

  let of_state s =
    let a = of_board s.board in
    let set_elf p _ = set a p 'E' in
    let set_gob p _ = set a p 'G' in
    PointMap.iter set_elf s.elves ;
    PointMap.iter set_gob s.elves ;
    a
  ;;

  let output ch (b : t) =
    let output_row r =
      Array.iter (output_char ch) r ;
      output_char ch '\n'
    in
    Array.iter output_row b
  ;;
end

let output_state ch s =
  for y = 0 to s.board.height do
    let units = ref [] in
    let add_unit s hp = units := (s, hp) :: !units in
    for x = 0 to s.board.width do
      let p : Point.t = { x; y } in
      match PointSet.find_opt p s.board.walls with
      | Some _ -> output_char ch '#'
      | None -> (
        match PointMap.find_opt p s.elves with
        | Some u -> output_char ch 'E' ; add_unit 'E' u.hp
        | None -> (
          match PointMap.find_opt p s.goblins with
          | Some u -> output_char ch 'G' ; add_unit 'G' u.hp
          | None -> output_char ch '.' ) )
    done ;
    output_string ch "   " ;
    if List.length !units > 0 then
      !units
      |> List.rev
      |> List.map (fun (s, hp) -> Printf.sprintf "%c(%d)" s hp)
      |> String.concat ", "
      |> output_string ch ;
    output_char ch '\n'
  done
;;

let rec char_seq_of_channel ch () =
  try Seq.Cons (input_char ch, char_seq_of_channel ch)
  with End_of_file -> Seq.Nil
;;

let rec pos_seq ?(p : Point.t = { x= 0; y= 0 }) (cs : char Seq.t) :
    (char * Point.t) Seq.t =
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

type tile = Empty | Wall | Elf of unit | Goblin of unit

let tile_of_char c =
  match c with
  | '.' -> Empty
  | '#' -> Wall
  | 'E' -> Elf default_unit
  | 'G' -> Goblin default_unit
  | _ -> invalid_arg (Printf.sprintf "Unexpected char '%c'" c)
;;

let char_of_tile t =
  match t with
  | Empty -> '.'
  | Wall -> '#'
  | Elf _ -> 'E'
  | Goblin _ -> 'G'
;;

module Bounds = struct
  type t = { min_x: int; max_x: int; min_y: int; max_y: int }

  let fold (bounds : t option) ({ x; y } : Point.t) =
    match bounds with
    | None -> Some { min_x= x; max_x= x; min_y= y; max_y= y }
    | Some { min_x; max_x; min_y; max_y } ->
        let min_x = min min_x x in
        let max_x = max max_x x in
        let min_y = min min_y y in
        let max_y = max max_y y in
        Some { min_x; max_x; min_y; max_y }
  ;;

  let of_seq (s : Point.t Seq.t) : t option = s |> Seq.fold_left fold None

  (** (width, height) of bounds *)
  let dimensions b = (b.max_x - b.min_x, b.max_y - b.min_y)
end

let parse_state (cs : char Seq.t) : state =
  let module StateFold = struct
    type t =
      { bounds: Bounds.t option
      ; walls: PointSet.t
      ; elves: unit PointMap.t
      ; goblins: unit PointMap.t
      }

    let empty =
      { bounds= None
      ; walls= PointSet.empty
      ; elves= PointMap.empty
      ; goblins= PointMap.empty
      }
    ;;

    let collect c (t, pos) =
      let c = { c with bounds= Bounds.fold c.bounds pos } in
      match t with
      | Empty -> c
      | Wall -> { c with walls= PointSet.add pos c.walls }
      | Elf e -> { c with elves= PointMap.add pos e c.elves }
      | Goblin g -> { c with goblins= PointMap.add pos g c.goblins }
    ;;

    let of_seq = Seq.fold_left collect empty

    let to_state { bounds; walls; elves; goblins } =
      let width, height =
        match bounds with
        | None -> invalid_arg "Empty bounds"
        | Some b -> (b.max_x - b.min_x, b.max_y - b.min_y)
      in
      { board= { width; height; walls }; elves; goblins }
    ;;
  end in
  let parse_char (c, p) : (tile * Point.t) option =
    try
      match c with
      | '\n' -> None
      | _ -> Some (tile_of_char c, p)
    with Invalid_argument e ->
      invalid_arg (Printf.sprintf "Line %d:%d: %s" (p.y + 1) (p.x + 1) e)
  in

  cs
  |> pos_seq
  |> Seq.filter_map parse_char
  |> StateFold.of_seq
  |> StateFold.to_state
;;

let get_input ch = ch |> char_seq_of_channel |> parse_state
