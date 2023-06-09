#! /usr/bin/env ocaml
(* Day 9 part 2: Marble Mania
 *
 * USAGE:
 *
 *    ./day09.ml < input.txt
 *
 * CONTEXT:
 *
 * Amused by the speed of your answer, the Elves are curious:
 * 
 * What would the new winning Elf's score be if the number of the last marble were 100 times larger?
 *)

let seq_viewer v = Seq.map (fun a -> v a; a);;

(** Get the last non-Nil item in a sequence *)
let seq_last (s: 'a Seq.t) : 'a =
  let rec seq_last' last (s: 'a Seq.t) =
    match s () with
    | Nil -> last
    | Cons (last', s') -> seq_last' last' s'
  in
  match s () with
  | Nil -> failwith "Sequence was empty"
  | Cons (last, s') -> seq_last' last s'
;;

(** a circular list *)
module Circle = struct
    type t = {
       next: int list; (** current center and next items *)
       prev: int list; (** previous items in reverse order *)
    }

    let empty : t = {next=[]; prev=[];};;
   
    (** insert a new item, centering around it *)
    let insert item c : t =
        { next=item::c.next; prev=c.prev; }
    ;;
    
    (** remove the current center of the list, fails if empty *)
    let remove c : int * t =
        match c.next with
        | hd::next' -> hd, {next=next'; prev=c.prev;}
        | [] -> match List.rev c.prev with
            | [] -> failwith "cannot remove from empty Circle"
            | hd::next' -> hd, {next=next'; prev=[];}
    ;;
    
    let rec shift_forward i c : t =
        if i = 0 then c else
        match c.next with
        | hd::next' -> shift_forward (i - 1) {next=next'; prev=hd::c.prev}
        | [] -> shift_forward i {prev=[]; next=(List.rev c.prev)}
    ;;

    let rec shift_backward i c : t =
        if i = 0 then c else
        match c.prev with
        | hd::prev' ->  shift_backward (i - 1) {next=hd::c.next; prev=prev'}
        | [] -> shift_backward i {prev=(List.rev c.next); next=[]}
    ;;
end

module IntMap = Map.Make(Int)

type state = {
    players: int;
    round: int;
    circle: Circle.t;
    scores: int IntMap.t; (** player -> score *)
}

let parse_ctx s =
    let parse_players s =
        match String.split_on_char ' ' s with
        | [players; "players"] -> int_of_string players
        | _ -> failwith "invalid players"
    and parse_marbles s =
        match s |> String.trim |> String.split_on_char ' ' with
        | ["last"; "marble"; "is"; "worth"; points; "points"] ->
            int_of_string points
        | _ -> failwith "invalid marbles"
    in

    match String.split_on_char ';' s with
    | players::marbles::_ -> parse_players players, parse_marbles (marbles |> String.split_on_char ':' |> List.hd)
    | _ -> failwith "invalid input"
;;

(** list indexing that supports negative indices and loops around *)
let nth_clockwise n circle =
    let len = Array.length circle in
    let n = n mod len in
    if n >= 0
    then n
    else n + len
;;

(** insert item before index i (the inserted item will now have index i) *)
let list_insert item i l : 'a array =
    if i < 0 || i >= Array.length l then failwith "index out of range for list" else
    Array.concat [(Array.sub l 0 i); [|item|]; (Array.sub l i (Array.length l - i))]
;;

let list_remove i l : 'a * 'a array =
    if i < 0 || i >= Array.length l then failwith "index out of range for list" else
    Array.get l i,
    if Array.length l - 1 = i then Array.sub l 0 i else
    Array.append (Array.sub l 0 i) (Array.sub l (i + 1) (Array.length l - i - 1))
;;

let add_score p s scores =
    IntMap.add p
    (match IntMap.find_opt p scores with
    | Some s' -> s' + s
    | None -> s)
    scores
;;

let current_player players round = (round - 1) mod players + 1;;

(** add a new marble to the circle, returning the new circle, the new current_i, and any points won *)
let play_round {
    players;
    round;
    circle;
    scores;
} : state =
    let round = round + 1 in
    let current_player = current_player players round in

    if round mod 23 = 0 then
	let removed, circle = circle |> Circle.shift_backward 7 |> Circle.remove in

        let scores = add_score current_player (round + removed) scores in
        { players; round; circle; scores; }
    else
        let circle = circle |> Circle.shift_forward 2 |> Circle.insert round in
        { players; round; circle; scores; }
;;

let rec play_for i s : state Seq.t = fun () ->
    if s.round >= i
    then
        Nil
    else
        let s' = play_round s in
        Cons (s', play_for i s')
;;

let high_score s =
    s |> IntMap.to_seq |> Seq.map snd |> Seq.fold_left max 0
;;

let output_scores ch s =
    s
    |> IntMap.to_seq
    |> Seq.map (function | (p, s) -> Printf.sprintf "%d: %d" p s)
    |> List.of_seq
    |> String.concat ", "
    |> Printf.fprintf ch "{ %s }\n"
;;

(* let output_board ch s =
    let { circle; current_i; players; round; } = s in
    let circle_str = circle
    |> Array.to_list
    |> List.map string_of_int
    |> List.mapi (fun i s -> if i = current_i then "(" ^ s ^ ")" else s)
    |> List.map ((^) "\t")
    |> String.concat ""
    in
    Printf.fprintf ch "[%d]%s\n" (current_player s.players s.round) circle_str;
;; *)

let () =
    let players, marbles = stdin |> input_line |> parse_ctx in

    let marbles = marbles * 100 in (* part 2 *)

    let state = {
        players;
        round = 0;
        circle = Circle.insert 0 Circle.empty;
        scores = IntMap.empty;
    } in

    Seq.cons state (play_for marbles state)
    (* |> seq_viewer (output_board stderr) (* (slower) *) *)
    |> seq_viewer (fun s -> Printf.eprintf "%d\n" s.round(* ; flush stderr *))
    (* |> seq_viewer (fun {scores;} -> output_scores stderr scores) *)
    |> seq_last
    |> (fun s ->
        (* output_scores stderr s.scores; *)
        flush stderr;
        high_score s.scores |> Printf.printf "%d\n")
;;
