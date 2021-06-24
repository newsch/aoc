#! /usr/bin/env -S ocaml str.cma

(*
> Strategy 2: Of all guards, which guard is most frequently asleep on the same
> minute?
> In the example above, Guard #99 spent minute 45 asleep more than any other
> guard or minute - three times in total. (In all other cases, any guard spent
> any minute asleep at most twice.)
 *)

(* utils *)

let rec seq_of_stream (stream : 'a Stream.t) : 'a Seq.t =
 fun () ->
  match Stream.peek stream with
  | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
  | None -> Nil

let line_seq_of_channel channel =
  seq_of_stream
    (Stream.from (fun _ ->
         try Some (input_line channel) with End_of_file -> None))

module Date = struct
  type t = int * int * int

  let create (year : int) (month : int) (day : int) : t option =
    Some (year, month, day)
end

type date = Date.t

module Time = struct
  type t = int * int

  let create (hours : int) (minutes : int) : t option = Some (hours, minutes)

  let diff older newer : int =
    let oh, om = older in
    let nh, nm = newer in
    ((nh * 60) + nm) - ((oh * 60) + om)
end

type time = Time.t

type datetime = date * time

(* meat & potatoes *)

type id = int

type log = NewGuard of id | Sleep | Wake

let parse_log (s : string) : (datetime * log, string) Result.t =
  (* Format: "[1518-05-24 23:56] Guard #1721 begins shift" | "[1518-08-22 00:09] falls asleep" | "[1518-05-19 00:53] wakes up" *)
  let log_r =
    Str.regexp "^\\[\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\)\\] \\(Guard #\\([0-9]+\\) begins shift\\|\\(falls asleep\\)\\|\\(wakes up\\)\\)"
    (*                (1: year )   (2: month)   (3: day  )   (4: hour ):  (5: min  )      (6        (7: id   )                  (8: asleep     )     (9: awake  )  ) *)
  in
  if Str.string_match log_r s 0 then
    let get_piece i = int_of_string_opt (Str.matched_group i s) in
    let group_found i =
      try if Str.group_beginning i = 0 then true else true
      with Not_found -> false
    in
    match get_piece 1 with
    | None -> Error "year not found"
    | Some year ->
        match get_piece 2 with
        | None -> Error "month not found"
        | Some month ->
            match get_piece 3 with
            | None -> Error "day not found"
            | Some day ->
                match get_piece 4 with
                | None -> Error "hour not found"
                | Some hour ->
                    match get_piece 5 with
                    | None -> Error "minute not found"
                    | Some minute ->
                        let dt : datetime =
                          ((year, month, day), (hour, minute))
                        in
                        if group_found 7 then
                          match get_piece 7 with
                          | None -> Error "guard id not found"
                          | Some id -> Ok (dt, NewGuard id)
                        else if group_found 8 then Ok (dt, Sleep)
                        else if group_found 9 then Ok (dt, Wake)
                        else Error "Couldn't parse message"
  else Error "line did not match"

type guard_record = { id : id; date : date; asleep : (time * time) list }

let time_asleep (g : guard_record) : int =
  List.fold_left (fun sum (t1, t2) -> sum + Time.diff t1 t2) 0 g.asleep

let make_records (ls : (datetime * log) Seq.t) : guard_record list =
  let add_wake_time (existing : guard_record list * time option)
      (wake_time : time) : guard_record list =
    match existing with
    | l, Option.None -> failwith "Tried to add wake time with no sleep time"
    | h :: l, Option.Some t ->
        { id = h.id; date = h.date; asleep = (t, wake_time) :: h.asleep } :: l
    | [], Option.Some _ -> failwith "Tried to add wake time to empty record list"
  in
  (* Group records by guard *)
  let group (existing : guard_record list * time option) (next : datetime * log)
      : guard_record list * time option =
    match next with
    (* Add new record to list *)
    | (date, time), NewGuard id ->
        (match existing with | e, _ ->
        ({ id; date; asleep = [] }::e, None))
    (* Update last record in list *)
    | (_date, time), Wake -> (add_wake_time existing time, None)
    | (_date, time), Sleep -> (
        match existing with e, None -> (e, Some time) | _ -> failwith "Tried to sleep with existing time" )
  in
  let records, _ = Seq.fold_left group ([], None) ls in
  records

let time_asleep (r : (time * time) list) : int =
  r
  |> List.map (fun (t1, t2) -> Time.diff t1 t2)
  |> List.fold_left ( + ) 0

let minute_most_asleep (r : (time * time) list) : (int * int) =
  (* Generate a sequence of [s, e) *)
  let rec expand_range (s: int) (e: int) : int Seq.t = fun () ->
    if s >= e
    then
      Nil
    else
      Cons (s, expand_range (s + 1) e)
  in
  r
  |> List.to_seq
  |> Seq.flat_map (function | ((0, s), (0, e)) -> expand_range s e | _ -> failwith "Unexpected time not at midnight")
  |> List.of_seq
  |> List.sort Int.compare
  |> List.fold_left (fun acc next -> match acc, next with | (a, i)::rest, b when a = b -> (a, i + 1)::rest | r, a -> (a, 1)::r) []
  |> List.sort (fun a b -> match a, b with | (_, i1), (_, i2) -> -Int.compare i1 i2)
  (* |> List.map (function | (a, i) -> Printf.eprintf "(%d, %d)\n" a i; (a, i)) *)
  |> function
    | hd::_ -> hd
    | [] -> (0, 0)

let main () =
  let line_count = ref 0 in
  let records =
    stdin |> line_seq_of_channel |> Seq.map parse_log
    |> Seq.map (fun m ->
           line_count := !line_count + 1;
           m)
    |> Seq.map (function
         | Ok c -> c
         | Error e ->
             Printf.sprintf "couldn't parse line %d: %s" !line_count e
             |> failwith)
    |> make_records
  in
  (* ids and times header *)
  (* Printf.printf "id\ttime asleep\n"; *)
  records
  |> List.sort (fun r1 r2 -> Int.compare r1.id r2.id)
  |> List.fold_left (fun rs r -> match rs with | (r_id, r_asleep)::rs' when r_id = r.id -> (r_id, r.asleep @ r_asleep)::rs' | _ -> (r.id, r.asleep)::rs) []
  |> List.map (function | (id, sleep) -> (id, minute_most_asleep sleep))
  |> List.sort (fun goat next -> let (_, (_, goat_asleep_most)) = goat in let (_, (_, next_asleep_most)) = next in Int.compare next_asleep_most goat_asleep_most)
  (* Print ids and times *)
  (* |> List.iter (fun a ->
         let a1, b1 = a in
         Printf.printf "%d\t%d\n" a1 (time_asleep b1)) *)
  |> List.hd
  (* |> function | id, (minute, num) -> Printf.eprintf "id: %d\tminute most asleep: %d\t count: %d\n" id minute num *)
  (* Print id * common minute asleep... *)
  |> function | id, (minute, _num) -> id * minute
  |> Printf.printf "%d\n"

let () =
  main ()
