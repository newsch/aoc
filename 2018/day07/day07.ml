#! /usr/bin/env ocaml
(* Day 7: The Sum of Its Parts
 *
 * USAGE:
 *
 *    ./day07.ml < input.txt
 *
 * CONTEXT:
 *
 * You find yourself standing on a snow-covered coastline; apparently, you
 * landed a little off course. The region is too hilly to see the North Pole
 * from here, but you do spot some Elves that seem to be trying to unpack
 * something that washed ashore. It's quite cold out, so you decide to risk
 * creating a paradox by asking them for directions.
 *
 * "Oh, are you the search party?" Somehow, you can understand whatever Elves
 * from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could
 * the device on your wrist also be a translator? "Those clothes don't look
 * very warm; take this." They hand you a heavy coat.
 *
 * "We do need to find our way back to the North Pole, but we have higher
 * priorities at the moment. You see, believe it or not, this box contains
 * something that will solve all of Santa's transportation problems - at
 * least, that's what it looks like from the pictures in the instructions." It
 * doesn't seem like they can read whatever language it's in, but you can:
 * "Sleigh kit. Some assembly required."
 *
 * "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh'
 * at once!" They start excitedly pulling more parts out of the box.
 *
 * The instructions specify a series of steps and requirements about which
 * steps must be finished before others can begin (your puzzle input). Each
 * step is designated by a single letter. For example, suppose you have the
 * following instructions:
 *
 * - Step C must be finished before step A can begin.
 * - Step C must be finished before step F can begin.
 * - Step A must be finished before step B can begin.
 * - Step A must be finished before step D can begin.
 * - Step B must be finished before step E can begin.
 * - Step D must be finished before step E can begin.
 * - Step F must be finished before step E can begin.
 *
 * Visually, these requirements look like this:
 *
 *       -->A--->B--
 *      /    \      \
 *     C      -->D----->E
 *      \           /
 *       ---->F-----
 *
 * Your first goal is to determine the order in which the steps should be
 * completed. If more than one step is ready, choose the step which is first
 * alphabetically. In this example, the steps would be completed as follows:
 *
 * - Only C is available, and so it is done first.
 * - Next, both A and F are available. A is first alphabetically, so it
 * is done next.
 * - Then, even though F was available earlier, steps B and D are now
 * also available, and B is the first alphabetically of the three.
 * - After that, only D and F are available. E is not available because
 * only some of its prerequisites are complete. Therefore, D is completed
 * next.
 * - F is the only choice, so it is done next.
 * - Finally, E is completed.
 *
 * So, in this example, the correct order is CABDFE.
 *
 * In what order should the steps in your instructions be completed?
 *)

(* make tree *)
(* start with root *)
(* track "seen" *)
(* of all seen, find the next "most eligible" available node *)
(* add node to seen and repeat *)

let line_seq_of_channel channel =
  let rec seq_of_stream (stream : 'a Stream.t) : 'a Seq.t = fun () ->
    match Stream.peek stream with
    | Some _ -> Cons (Stream.next stream, seq_of_stream stream)
    | None -> Nil
  in
  seq_of_stream
    (Stream.from (fun _ ->
         try Some (input_line channel) with End_of_file -> None))

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

let rec find_steps (seen: CharSet.t) (m: depmap) : char Seq.t = fun () ->
  let priority_order = Char.compare in
  m
  |> CharMap.to_seq
  |> Seq.filter (function | node, deps -> CharSet.subset deps seen)
  |> Seq.map fst
  |> List.of_seq
  |> List.sort priority_order
  |> function
    | [] -> Seq.Nil
    | hd::_ -> Seq.Cons (hd, (find_steps (CharSet.add hd seen) (CharMap.remove hd m)))

let print_map (m: depmap) =
  let string_of_char c : string = String.of_seq (Seq.cons c Seq.empty) in
  let set_repr = fun s -> s |> CharSet.to_seq |> Seq.fold_left (fun str el -> str ^ string_of_char el ^ "; ") "{" |> fun s -> s ^ "}"; in
  let print_key = fun node deps -> Printf.eprintf "  %c : %s\n" node (set_repr deps) in
  output_string stderr "{\n";
  CharMap.iter print_key m;
  output_string stderr "}\n";
;;

(* output a plantuml diagram of the dependencies
 *
 * Usage:
 *     ./day07.ml < input.txt | plantuml -p > viz.png
 *)
let viz_plantuml () =
  let edges =
    stdin
    |> line_seq_of_channel
    |> Seq.map parse_edge
    |> List.of_seq
  in
  output_string stdout "@startuml\nhide empty description\n[*] --> [*]\n";
  edges |> List.iter (function | node, dep -> Printf.printf "%c --> %c\n" dep node );
  output_string stdout "@enduml\n";
;;

(* output a graphviz diagram of the dependencies
 *
 * Usage:
 *     ./day07.ml < input.txt | dot -Tsvg > viz.svg
 *)
let viz_graphviz () =
  let edges =
    stdin
    |> line_seq_of_channel
    |> Seq.map parse_edge
    |> List.of_seq
  in
  output_string stdout "digraph {\n";
  edges |> List.iter (function | node, dep -> Printf.printf "%c -> %c;\n" dep node );
  output_string stdout "}\n";
;;

let main () =
  stdin
  |> line_seq_of_channel
  |> Seq.map parse_edge
  (* |> Seq.iter (function | n, d -> Printf.eprintf "(%c, %c)\n" n d) *)
  |> build_map
  (* |> print_map; *)
  |> find_steps CharSet.empty
  |> Seq.iter (output_char stdout)
  ;
  output_char stdout '\n'
;;

let () =
  (* main () *)
  viz_graphviz ()
  (* viz_plantuml () *)
;;
