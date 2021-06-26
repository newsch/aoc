#! /usr/bin/env ocaml
(* Day 7 Part 2: The Sum of Its Parts
 *
 * USAGE:
 *
 *    ./day07p2.ml < input.txt
 *
 * CONTEXT:
 *
 * As you're about to begin construction, four of the Elves offer to help. "The
 * sun will set soon; it'll go faster if we work together." Now, you need to
 * account for multiple people working on steps simultaneously. If multiple
 * steps are available, workers should still begin them in alphabetical order.
 *
 * Each step takes 60 seconds plus an amount corresponding to its letter:
 * A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step
 * Z takes 60+26=86 seconds. No time is required between steps.
 *
 * To simplify things for the example, however, suppose you only have help from
 * one Elf (a total of two workers) and that each step takes 60 fewer seconds
 * (so that step A takes 1 second and step Z takes 26 seconds). Then, using
 * the same instructions as above, this is how each second would be spent:
 *
 *     Second   Worker 1   Worker 2   Done
 *        0        C          .
 *        1        C          .
 *        2        C          .
 *        3        A          F       C
 *        4        B          F       CA
 *        5        B          F       CA
 *        6        D          F       CAB
 *        7        D          F       CAB
 *        8        D          F       CAB
 *        9        D          .       CABF
 *       10        E          .       CABFD
 *       11        E          .       CABFD
 *       12        E          .       CABFD
 *       13        E          .       CABFD
 *       14        E          .       CABFD
 *       15        .          .       CABFDE
 *
 * Each row represents one second of time. The Second column identifies how
 * many seconds have passed as of the beginning of that second. Each worker
 * column shows the step that worker is currently doing (or . if they are
 * idle). The Done column shows completed steps.
 *
 * Note that the order of the steps has changed; this is because steps now take
 * time to finish and multiple workers can begin multiple steps simultaneously.
 *
 * In this example, it would take 15 seconds for two workers to complete
 * these steps.
 *
 * With 5 workers and the 60+ second step durations described above, how
 * long will it take to complete all of the steps?
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

(* Get the last non-Nil item in a sequence *)
let seq_last (s: 'a Seq.t) : 'a =
  let rec seq_last' last (s: 'a Seq.t) =
    match s () with
    | Nil -> last
    | Cons (last', s') -> seq_last' last' s'
  in
  match s () with
  | Nil -> failwith "Sequence was empty"
  | Cons (last, s') -> seq_last' last s'

module CharSet = Set.Make(Char)

module CharMap = Map.Make(Char)

type depmap = CharSet.t CharMap.t

let parse_edge s =
  if not (
    String.sub s 0 5 = "Step "
    && String.sub s 6 30 = " must be finished before step "
    && String.sub s 37 11 = " can begin."
  ) then failwith "bad format" else
  let dep = String.get s 5
  and node = String.get s 36
  in
  (node, dep)

(* Make a tree stored as a map of node -> dependencies *)
let build_map s : depmap =
  Seq.fold_left (fun m (node, dep) ->
    (* Ensure dep is in map *)
    let m = match CharMap.find_opt dep m with
    | None -> CharMap.add dep CharSet.empty m
    | Some _ -> m
    in

    match CharMap.find_opt node m with
    | None -> CharMap.add node (CharSet.singleton dep) m
    | Some deps -> CharMap.add node (CharSet.add dep deps) m
  ) CharMap.empty s

(* A task and how long remains on it *)
type worker = (char * int) option

type state = {
  time: int;
  workers: worker array;
  finished: char list;
  remaining: depmap;
}

let work_available finished remaining : char list =
  let priority_order = Char.compare in
  let finished = finished |> List.to_seq |> CharSet.of_seq in

  remaining
  |> CharMap.to_seq
  |> Seq.filter (function | node, deps -> CharSet.subset deps finished)
  |> Seq.map fst
  |> List.of_seq
  |> List.sort priority_order
;;

let simulate {time; workers; finished; remaining;} : state =
  let work_time (w: char) : int =
    if w < 'A' || w > 'Z' then raise (Invalid_argument "bad work char") else
    let base_time = 60 in
    Char.code w - Char.code 'A' + 1 + base_time
  in

  (* Advance time and get completed work *)
  let time = time + 1 in
  let workers = workers |> Array.map (Option.map (function | w, t -> w, t-1)) in
  let just_completed = workers |> Array.to_seq |> Seq.filter_map (function | Some (w, 0) -> Some w | _ -> None) |> List.of_seq in
  let workers = workers |> Array.map (function | Some (w, t) when t <= 0 -> None | o -> o) in
  let finished = List.append finished just_completed in
  (* Assign new work *)
  let available = ref (work_available finished remaining) in
  let workers = workers |> Array.map (function
    | Some s -> Some s
    | None -> match !available with
      | [] -> None
      | w::rs' -> available := rs'; Some (w, work_time w)
  ) in
  (* Update remaining *)
  let remaining = remaining |> CharMap.filter (fun node deps ->
    workers |> Array.exists (function | Some (n, _) when n = node -> true | _ -> false) |> not (* node is being worked on *)
  ) in

  { time; workers; finished; remaining; }
;;

let rec run_until_done (s: state) : state Seq.t = fun () ->
  let remaining' = s.remaining |> CharMap.to_seq |> Seq.map fst |> CharSet.of_seq
  and finished' = s.finished |> List.to_seq |> CharSet.of_seq
  in

  if CharSet.subset remaining' finished' && Array.for_all Option.is_none s.workers then Nil else
  let s' = simulate s in
  Cons (s', run_until_done s')
;;

let output_state_header ch (s: state) =
  output_string ch "Time";
  Array.iteri (fun i _ -> Printf.fprintf ch "\tWork %d" i) s.workers;
  output_string ch "\tAvail";
  output_string ch "\tDone";
  output_string ch "\tRemaining";
  output_char ch '\n';
;;

let output_state ch (s: state) =
  let list_to_str = fun l -> l |> List.to_seq |> String.of_seq in
  Printf.fprintf ch "%d" s.time;
  Array.iter (fun w -> Printf.fprintf ch "\t%s" (match w with | None -> "." | Some (w, t) -> Printf.sprintf "%c(%d)" w t)) s.workers;
  output_char ch '\t';
  output_string ch (work_available s.finished s.remaining |> list_to_str);
  output_char ch '\t';
  output_string ch (s.finished |> list_to_str);
  output_char ch '\t';
  output_string ch (s.remaining |> CharMap.to_seq |> Seq.map fst |> String.of_seq);
  output_char ch '\n';
;;

let make_state () =
  let work =
  stdin
  |> line_seq_of_channel
  |> Seq.map parse_edge
  (* |> Seq.iter (function | n, d -> Printf.eprintf "(%c, %c)\n" n d) *)
  |> build_map
  (* |> print_map; *)
  in
  {
    time = -1;
    workers = Array.make 5 None;
    finished = [];
    remaining = work;
  }
;;

(* Print a state table similar to prompt *)
let viz () =
  let state = make_state () in

  output_state_header stderr state;

  run_until_done state
  |> Seq.iter (fun s -> output_state stderr s)
;;

(* Print the total number of seconds required to complete *)
let main () =
  let state = make_state () in

  run_until_done state
  |> seq_last
  |> (fun s -> Printf.printf "%d\n" s.time)
;;

let () =
  main ()
  (* viz () *)
;;
