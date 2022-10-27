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

let rec char_seq_of_channel ch () =
  try Seq.Cons (input_char ch, char_seq_of_channel ch)
  with End_of_file -> Seq.Nil
;;

let char_seq_of_string s =
  let stop = String.length s in
  let rec loop i =
    if i >= stop then Seq.empty else Seq.cons (String.get s i) (loop (i + 1))
  in
  loop 0
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

type unit = { hp: int; ap: int }

type unit_kind = Elf | Goblin

let enemy_kind = function
  | Elf -> Goblin
  | Goblin -> Elf
;;

let default_unit = { hp= 200; ap= 3 }

type tile = Empty | Wall | Unit of (unit_kind * unit)

let tile_of_char c =
  match c with
  | '.' -> Empty
  | '#' -> Wall
  | 'E' -> Unit (Elf, default_unit)
  | 'G' -> Unit (Goblin, default_unit)
  | _ -> invalid_arg (Printf.sprintf "Unexpected char '%c'" c)
;;

let char_of_tile t =
  match t with
  | Empty -> '.'
  | Wall -> '#'
  | Unit (Elf, _) -> 'E'
  | Unit (Goblin, _) -> 'G'
;;

module PointSet = Set.Make (Point)
module PointMap = Map.Make (Point)

module PointVertex = struct
  type t = Point.t * Point.t

  let is_normalized (p1, p2) = Point.compare p1 p2 <= 0

  (** Order the pair so that the lesser point (by Point.compare) comes first *)
  let normalize v : t =
    if is_normalized v then v
    else
      (* swap *)
      let p1, p2 = v in
      (p2, p1)
  ;;

  (** Compare for already normalized vertices *)
  let compare v1 v2 : int =
    assert (is_normalized v1) ;
    assert (is_normalized v2) ;
    let (v1p1, v1p2), (v2p1, v2p2) = (v1, v2) in
    match Point.compare v1p1 v2p1 with
    | 0 -> Point.compare v1p2 v2p2
    | i -> i
  ;;
end

module State = struct
  type t =
    { height: int
    ; width: int
    ; walls: PointSet.t
    ; elves: unit PointMap.t
    ; goblins: unit PointMap.t
    }

  let empty =
    { height= 0
    ; width= 0
    ; walls= PointSet.empty
    ; elves= PointMap.empty
    ; goblins= PointMap.empty
    }
  ;;

  (** Ascending persistent sequence of elves *)
  let elves_seq s = PointMap.to_seq s.elves

  (** Ascending persistent sequence of goblins *)
  let goblins_seq s = PointMap.to_seq s.goblins

  (** Ascending persistent sequence of elves and goblins *)
  let units_seq s : (Point.t * (unit_kind * unit)) Seq.t =
    let cmp_pos (p1, _) (p2, _) = Point.compare p1 p2
    and elves = elves_seq s |> Seq.map (fun (p, e) -> (p, (Elf, e)))
    and goblins = goblins_seq s |> Seq.map (fun (p, g) -> (p, (Goblin, g))) in

    Seq.sorted_merge cmp_pos elves goblins
  ;;

  let add (s : t) (p : Point.t) tile : t =
    let s = if p.x >= s.width then { s with width= p.x + 1 } else s in
    let s = if p.y >= s.height then { s with height= p.y + 1 } else s in
    match tile with
    | Empty -> s
    | Wall -> { s with walls= PointSet.add p s.walls }
    | Unit (Elf, e) -> { s with elves= PointMap.add p e s.elves }
    | Unit (Goblin, e) -> { s with goblins= PointMap.add p e s.goblins }
  ;;

  let is_in s (p : Point.t) : bool =
    p.x >= 0 && p.x < s.width && p.y >= 0 && p.y < s.height
  ;;

  let get s p : tile =
    assert (is_in s p) ;
    match PointSet.find_opt p s.walls with
    | Some _ -> Wall
    | None -> (
      match PointMap.find_opt p s.elves with
      | Some e -> Unit (Elf, e)
      | None -> (
        match PointMap.find_opt p s.goblins with
        | Some g -> Unit (Goblin, g)
        | None -> Empty ) )
  ;;

  let get_unit s p : (unit_kind * unit) option =
    match PointMap.find_opt p s.elves with
    | Some e -> Some (Elf, e)
    | None -> (
      match PointMap.find_opt p s.goblins with
      | Some g -> Some (Goblin, g)
      | None -> None )
  ;;

  let remove_unit s p : t =
    match PointMap.find_opt p s.elves with
    | Some _e -> { s with elves= PointMap.remove p s.elves }
    | None -> (
      match PointMap.find_opt p s.goblins with
      | Some _g -> { s with goblins= PointMap.remove p s.goblins }
      | None -> s )
  ;;

  let update_unit s p u : t =
    match PointMap.find_opt p s.elves with
    | Some _e -> { s with elves= PointMap.add p u s.elves }
    | None -> (
      match PointMap.find_opt p s.goblins with
      | Some _g -> { s with goblins= PointMap.add p u s.goblins }
      | None -> s )
  ;;

  (** Ascending persistent sequence of opposing units *)
  let get_enemies = function
    | Elf -> goblins_seq
    | Goblin -> elves_seq
  ;;

  let is_empty s p : bool =
    match get p s with
    | Empty -> true
    | _ -> false
  ;;

  (** Ascending persistent sequence of squares around a point *)
  let around s (p : Point.t) =
    [ { p with y= p.y - 1 }
    ; { p with x= p.x - 1 }
    ; { p with x= p.x + 1 }
    ; { p with y= p.y + 1 }
    ]
    |> List.to_seq
    |> Seq.filter (is_in s)
    |> Seq.map (fun p -> (p, get s p))
  ;;

  (** Ascending persistent sequence of open squares around a point *)
  let open_around s (p : Point.t) : Point.t Seq.t =
    around s p
    |> Seq.filter_map (function
         | p, Empty -> Some p
         | _ -> None )
  ;;

  (** Ascending persistent sequence of squares with enemies of [kind] around a point *)
  let enemies_around s kind (p : Point.t) =
    around s p
    |> Seq.filter_map (function
         | p, Unit (other_kind, u) ->
             if other_kind != kind then Some (p, u) else None
         | _ -> None )
  ;;

  let output ch s =
    for y = 0 to s.height - 1 do
      let units = ref [] in
      let add_unit s = units := s :: !units in

      for x = 0 to s.width - 1 do
        let p : Point.t = { x; y } in
        let tile = get s p in
        output_char ch (char_of_tile tile) ;
        match get s p with
        | Unit u -> add_unit u
        | _ -> ()
      done ;

      output_string ch "   " ;

      if List.length !units > 0 then
        !units
        |> List.rev
        |> List.map (fun u ->
               Printf.sprintf "%c(%d)" (char_of_tile (Unit u)) (snd u).hp )
        |> String.concat ", "
        |> output_string ch ;

      output_char ch '\n'
    done
  ;;

  let parse (cs : char Seq.t) : t =
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
    |> Seq.fold_left (fun s (t, pos) -> add s pos t) empty
  ;;
end

type state = State.t

(** Ordering for units that prefers low health over position *)
let cmp_unit (p1, u1) (p2, u2) : int =
  match Int.compare u1.hp u2.hp with
  | 0 -> Point.compare p1 p2
  | i -> i
;;

type fight_outcome = Injured | Killed

type turn_end_reason =
  | UnitNotFound
  | NoEnemies
  | NoFreeSpaces
  | NoPath
  | Fought of fight_outcome
  | Moved
  | MovedAndFought of fight_outcome

type stop_reason = NoEnemies | Stalemate

let fight s atk_p def_p : fight_outcome * state =
  let atk = State.get_unit s atk_p and def = State.get_unit s def_p in
  (* assert both exist *)
  assert (Option.is_some atk) ;
  assert (Option.is_some def) ;
  let atk_kind, atk = Option.get atk and def_kind, def = Option.get def in
  (* assert are enemies *)
  assert (atk_kind != def_kind) ;

  let def = { def with hp= def.hp - atk.ap } in

  if def.hp <= 0 then (Killed, State.remove_unit s def_p)
  else (Injured, State.update_unit s def_p def)
;;

let step_unit (p : Point.t) (s : state) : state * turn_end_reason =
  match State.get_unit s p with
  | None -> (* Skip dead units *) (s, UnitNotFound)
  | Some (kind, u) -> (
      let targets = State.get_enemies kind s in
      if Seq.is_empty targets then (* Stop on no targets *) (s, NoEnemies)
      else
        (* jump to combat if next to one already *)
        let nearby_enemies =
          State.enemies_around s kind p |> List.of_seq |> List.sort cmp_unit
        in
        match nearby_enemies with
        | (ep, _) :: _ ->
            let res, s = fight s p ep in
            (s, Fought res)
        | [] ->
            (* Target selection *)
            let available_spaces =
              targets
              |> Seq.map fst
              |> Seq.map (State.open_around s)
              |> Seq.fold_left (Seq.sorted_merge Point.compare) Seq.empty
            in
            if Seq.is_empty available_spaces then
              (* Stop if no available spaces around targets *) (s, NoFreeSpaces)
            else (* Pathfinding *)
              failwith "Not Implemented" )
;;

(** Execute one round.

    @return the new state. If combat ends (one side is all dead), or no
    moves are made, [Error] is returned with the state of the (partially
    completed) round.

    Calling [step] on a returned [Err state] will result in the same state
    being returned. *)
let step (s : state) :
    state * turn_end_reason list * (Unit.t, stop_reason) result =
  (* for each unit (in reading order):
     - find all targets
       - if none, combat ends
     - find all open squares around targets
       - if no open squares, end turn
       - if already in range of target, continue to attack
     - determine square that can be reached in fewest number of steps (move up, down, left, right)
       - if multiple closest options, prefer by reading order of destination square
     - Take 1 step along shortest path to square
       - if multiple shortest paths, prefer by reading order of first step
     - Redetermine target by comparing adjacent enemies, prefer by lowest hp, then by reading order
     - Attack target by subtracting unit's attack power from target's hp
       - If target's hit points are 0 or lower, remove from game (and target will not take turn)
  *)
  let units = State.units_seq s |> Seq.map fst |> List.of_seq in

  let rec unit_loop units s' turn_outcomes =
    match units with
    | [] -> (s', turn_outcomes, if s' = s then Error Stalemate else Ok ())
    | u :: rest -> (
        let new_s, outcome = step_unit u s' in
        let new_outcomes = outcome :: turn_outcomes in
        (* Early exit on Error *)
        match outcome with
        | NoEnemies -> (new_s, new_outcomes, Error NoEnemies)
        | _ -> unit_loop rest new_s new_outcomes )
  in

  unit_loop units s []
;;

module StrBoard = struct
  (* array matrix of y, x *)
  type t = char array array

  let set b (p : Point.t) ch = b.(p.y).(p.x) <- ch

  let make w h : t = Array.make_matrix h w '.'

  let of_state (s : State.t) =
    let a = make s.width s.height in

    let set_wall p = set a p '#'
    and set_elf p _ = set a p 'E'
    and set_gob p _ = set a p 'G' in

    PointSet.iter set_wall s.walls ;
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

let get_input ch = ch |> char_seq_of_channel |> State.parse
