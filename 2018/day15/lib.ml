(* Printing *)
let pp_comma ppf () = Format.fprintf ppf ",@ "

let pp_semicolon ppf () = Format.fprintf ppf ";@ "

let pp_list ppf pp_v l =
  Format.fprintf ppf "[@[%a@]]"
    Format.(pp_print_list ~pp_sep:pp_semicolon pp_v)
    l
;;

let pp_set ppf pp_v s =
  Format.fprintf ppf "{@[%a@]}" Format.(pp_print_seq ~pp_sep:pp_comma pp_v) s
;;

let pp_map ppf pp_k pp_v (s : ('a * 'b) Seq.t) =
  let pp_pair ppf (k, v) = Format.fprintf ppf "%a => %a" pp_k k pp_v v in
  Format.fprintf ppf "{@[%a@]}" Format.(pp_print_seq ~pp_sep:pp_comma pp_pair) s
;;

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

let seq_viewer v = Seq.map (fun a -> v a ; a)

let seq_viewer_if c v s = if c then seq_viewer v s else s

(** An infinite sequence of ints that increments by one, starting at n *)
let rec seq_count n : int Seq.t = fun () -> Seq.Cons (n, seq_count (n + 1))

let seq_enumerate ?(start = 0) s = Seq.zip (seq_count start) s

let rec seq_stop_after cond s () =
  let open Seq in
  match s () with
  | Nil -> Nil
  | Cons (o, s') -> Cons (o, if cond o then empty else seq_stop_after cond s')
;;

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

(** inclusive sequence of start -> stop *)
let rec range start stop : int Seq.t =
 fun () ->
  if start > stop then Seq.Nil else Seq.Cons (start, range (start + 1) stop)
;;

module Point = struct
  type t = { x: int; y: int }

  (** compare for point following reading order (left to right, top to bottom) *)
  let compare p1 p2 =
    match (Int.compare p1.x p2.x, Int.compare p1.y p2.y) with
    | 0, 0 -> 0
    | x, 0 -> x
    | _, y -> y
  ;;

  let pp ppf p = Format.fprintf ppf "(%d, %d)" p.x p.y

  let arround p () =
    Seq.(
      Cons
        ( { p with y= p.y - 1 }
        , fun () ->
            Cons
              ( { p with x= p.x - 1 }
              , fun () ->
                  Cons
                    ( { p with x= p.x + 1 }
                    , fun () -> Cons ({ p with y= p.y + 1 }, fun () -> Nil) ) )
        ))
  ;;
end

(** [sorted_difference cmp xs ys] is the sorted sequence [xs] without the sorted [ys] *)
let rec seq_sorted_difference cmp xs ys () =
  let open Seq in
  match (xs (), ys ()) with
  | Nil, _ -> Nil
  | xs, Nil -> xs
  | Cons (x, xs'), Cons (y, ys') -> (
    match cmp x y with
    (* Skip matched items *)
    | 0 -> seq_sorted_difference cmp xs' ys ()
    (* Allow lesser items *)
    | lt when lt < 0 -> Cons (x, seq_sorted_difference cmp xs' ys)
    (* Check against next y for greater items *)
    | gt -> seq_sorted_difference cmp xs ys' () )
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

let pp_unit ppf u = Format.fprintf ppf "{hp=%d; ap=%d}" u.hp u.ap

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

let pp_pointset ppf s = pp_set ppf Point.pp (s |> PointSet.to_seq)

let pp_pointmapunit ppf m = pp_map ppf Point.pp pp_unit (m |> PointMap.to_seq)

let pp_pointmapint ppf m =
  pp_map ppf Point.pp Format.pp_print_int (m |> PointMap.to_seq)
;;

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
    let cmp_pos (p1, (k1, _)) (p2, (k2, _)) = Point.compare p1 p2
    and elves = elves_seq s |> Seq.map (fun (p, e) -> (p, (Elf, e)))
    and goblins = goblins_seq s |> Seq.map (fun (p, g) -> (p, (Goblin, g))) in

    Seq.sorted_merge cmp_pos elves goblins
  ;;

  let points_seq s =
    let xs = range 0 (s.width - 1) and ys = range 0 (s.height - 1) in
    Seq.flat_map (fun y -> Seq.map (fun x -> { Point.x; y }) xs) ys
  ;;

  let blocked_seq s : Point.t Seq.t =
    let key_seq s = s |> PointMap.to_seq |> Seq.map fst in
    s.walls
    |> PointSet.to_seq
    |> Seq.sorted_merge Point.compare (key_seq s.elves)
    |> Seq.sorted_merge Point.compare (key_seq s.goblins)
  ;;

  let empty_seq s : Point.t Seq.t =
    seq_sorted_difference Point.compare (points_seq s) (blocked_seq s)
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

  let update_unit s p np u : t =
    let update m =
      m
      |> (if np != p then PointMap.remove p else fun m -> m)
      |> PointMap.add np u
    in

    match PointMap.find_opt p s.elves with
    | Some _e -> { s with elves= update s.elves }
    | None -> (
      match PointMap.find_opt p s.goblins with
      | Some _g -> { s with goblins= update s.goblins }
      | None -> s )
  ;;

  let map_units f s =
    s
    |> units_seq
    |> seq_enumerate
    |> Seq.map (fun (i, (p, (k, u))) -> (p, f i p k u))
    |> Seq.fold_left (fun s (p, u) -> update_unit s p p u) s
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
    p
    |> Point.arround
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

let cmp_point_by_dist (p1, d1) (p2, d2) : int =
  match Int.compare d1 d2 with
  | 0 -> Point.compare p1 p2
  | i -> i
;;

type fight_outcome = Injured | Killed

type turn_end_reason =
  | UnitNotFound
  | NoEnemies
  | NoFreeSpaces
  | NoPath
  | Fought of (fight_outcome * unit_kind)
  | Moved
  | MovedAndFought of (fight_outcome * unit_kind)

type stop_reason = NoEnemies | Stalemate

module PQueue = struct
  module IntMap = Map.Make (Int)

  type 'a t = 'a list IntMap.t

  let empty = IntMap.empty

  let is_empty q = IntMap.is_empty q

  let add q p v =
    IntMap.update p
      (function
        | None -> Some [ v ]
        | Some vs -> Some (v :: vs) )
      q
  ;;

  let add_seq q p = Seq.fold_left (fun q v -> add q p v) q

  let get q =
    let p, vs = IntMap.min_binding q in
    match vs with
    | [] -> failwith "Unexpected empty list"
    | [ v ] -> ((p, v), IntMap.remove p q)
    | v :: vs' -> ((p, v), IntMap.add p vs' q)
  ;;
end

let calc_distance s from : int PointMap.t =
  (* Naive:
     - start with destination and dist 0
     - for each neighbor not in seen, add to seen with dist + 1
     - do for entire board
     - use map of all empty spaces?

     Optimised:
     - use priority queue of (dist, point Queue.t)
     - once found neighboring tile of destination, stop once all tiles in queue
       at that distance have been searched
  *)
  let empty = PointSet.of_seq (State.empty_seq s) in

  let rec walk queue seen =
    if PQueue.is_empty queue then seen
    else
      let (dist, node), queue = PQueue.get queue in
      let seen = PointMap.add node dist seen in

      let unseen_arround =
        node
        |> Point.arround
        |> Seq.filter (fun p -> PointSet.mem p empty)
        |> Seq.filter (fun p -> not (PointMap.mem p seen))
        |> List.of_seq
      in

      let seen =
        PointMap.add_seq
          (unseen_arround |> List.to_seq |> Seq.map (fun p -> (p, dist + 1)))
          seen
      and queue =
        PQueue.add_seq queue (dist + 1) (unseen_arround |> List.to_seq)
      in

      walk queue seen
  in

  let queue = PQueue.add PQueue.empty 0 from in
  let seen = PointMap.empty in

  walk queue seen
;;

let fight s atk_p def_p : (fight_outcome * unit_kind) * state =
  let atk = State.get_unit s atk_p and def = State.get_unit s def_p in
  (* assert both exist *)
  assert (Option.is_some atk) ;
  assert (Option.is_some def) ;
  let atk_kind, atk = Option.get atk and def_kind, def = Option.get def in
  (* assert are enemies *)
  assert (atk_kind != def_kind) ;

  let def = { def with hp= def.hp - atk.ap } in

  if def.hp <= 0 then ((Killed, def_kind), State.remove_unit s def_p)
  else ((Injured, def_kind), State.update_unit s def_p def_p def)
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
        | [] -> (
            (* Target selection *)
            let available_spaces =
              targets
              |> Seq.map fst
              |> Seq.map (State.open_around s)
              |> Seq.fold_left (Seq.sorted_merge Point.compare) Seq.empty
            in
            if Seq.is_empty available_spaces then
              (* Stop if no available spaces around targets *) (s, NoFreeSpaces)
            else
              (* Pathfinding *)
              let distances_from_unit = calc_distance s p in
              let space_distances =
                available_spaces
                |> Seq.filter_map (fun p ->
                       PointMap.find_opt p distances_from_unit
                       |> Option.map (fun d -> (p, d)) )
                |> List.of_seq
                |> List.sort cmp_point_by_dist
              in
              match space_distances with
              | [] -> (* Stop if no path to any spaces *) (s, NoPath)
              | (target, d) :: rest -> (
                  let distances_from_target = calc_distance s target in
                  let step_distances =
                    State.open_around s p
                    |> Seq.filter_map (fun p ->
                           PointMap.find_opt p distances_from_target
                           |> Option.map (fun d -> (p, d)) )
                    |> List.of_seq
                    |> List.sort cmp_point_by_dist
                  in
                  match step_distances with
                  | [] -> failwith "Could not find path again"
                  | (step, d) :: rest -> (
                      let s = State.update_unit s p step u and p = step in
                      (* combat if next to one *)
                      let nearby_enemies =
                        State.enemies_around s kind p
                        |> List.of_seq
                        |> List.sort cmp_unit
                      in
                      match nearby_enemies with
                      | [] -> (s, Moved)
                      | (ep, enemy) :: _ ->
                          let res, s = fight s p ep in
                          (s, MovedAndFought res) ) ) ) )
;;

(** Execute one round.

    @return the new state. If combat ends (one side is all dead), or no
    moves are made, [Error] is returned with the state of the (partially
    completed) round.

    Calling [step] on a returned [Err state] will result in the same state
    being returned. *)
let step (s : state) : state * turn_end_reason list * stop_reason option =
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
    | [] -> (s', turn_outcomes, if s' = s then Some Stalemate else None)
    | u :: rest -> (
        let new_s, outcome = step_unit u s' in
        let new_outcomes = outcome :: turn_outcomes in
        (* Early exit on Error *)
        match outcome with
        | NoEnemies -> (new_s, new_outcomes, Some NoEnemies)
        | _ -> unit_loop rest new_s new_outcomes )
  in

  unit_loop units s []
;;

let step_forever start =
  (start, [], None) |> Seq.iterate (fun (s, _, _) -> step s)
;;

let step_to_completion start =
  start
  |> step_forever
  |> seq_stop_after (fun (_, _, stop) -> Option.is_some stop)
;;

let step_to_death_or_completion start =
  let elf_died (_, log, _) =
    log
    |> List.find_opt (function
         | Fought (Killed, Elf) | MovedAndFought (Killed, Elf) -> true
         | _ -> false )
    |> Option.is_some
  in

  let complete (_, _, stop) = Option.is_some stop in

  let map_to_result res =
    let state, _, stop = res in
    ( state
    , match (elf_died res, stop) with
      | false, Some r -> Ok r
      | _, _ -> Error () )
  in

  start
  |> step_forever
  |> seq_stop_after elf_died
  |> seq_stop_after complete
  |> Seq.map map_to_result
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
    PointMap.iter set_gob s.goblins ;
    a
  ;;

  let add_distances d b =
    let char_of_num d = char_of_int (d + 48) in
    let char_of_dist d =
      assert (d >= 0) ;
      match d with
      | 0 -> 'X'
      | d when d >= 10 -> char_of_num (d mod 10)
      | d -> char_of_num d
    in
    d |> PointMap.iter (fun p d -> set b p (char_of_dist d)) ;
    b
  ;;

  let format fmt b =
    let output_row r =
      Array.iter (Format.pp_print_char fmt) r ;
      Format.pp_print_char fmt '\n'
    in
    Array.iter output_row b ;
    Format.pp_print_newline fmt ()
  ;;

  let output ch = format (Format.formatter_of_out_channel ch)

  let to_str b =
    format Format.str_formatter b ;
    Format.flush_str_formatter ()
  ;;
end

let get_input ch = ch |> char_seq_of_channel |> State.parse

let get_output rounds s =
  let total_hp =
    s
    |> State.units_seq
    |> Seq.map (fun (_, (_, u)) -> u.hp)
    |> Seq.fold_left ( + ) 0
  in
  rounds * total_hp
;;
