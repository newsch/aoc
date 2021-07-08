#! /usr/bin/env ocaml
(* Day 9: Marble Mania
 *
 * USAGE:
 *
 *    ./day09.ml < input.txt
 *
 * for viewing progress with pv in fish:
 *
 *    ./day09.ml < input.txt > output.txt 2>| pv -l -e -p -s $NUM_ROUNDS > /dev/null
 *
 * CONTEXT:
 *
 * You talk to the Elves while you wait for your navigation system to
 * initialize. To pass the time, they introduce you to their favorite
 * marble game.
 *
 * The Elves play this game by taking turns arranging the marbles in a circle
 * according to very particular rules. The marbles are numbered starting
 * with 0 and increasing by 1 until every marble has a number.
 *
 * First, the marble numbered 0 is placed in the circle. At this point,
 * while it contains only a single marble, it is still a circle: the marble
 * is both clockwise from itself and counter-clockwise from itself. This
 * marble is designated the current marble.
 *
 * Then, each Elf takes a turn placing the lowest-numbered remaining marble
 * into the circle between the marbles that are 1 and 2 marbles clockwise
 * of the current marble. (When the circle is large enough, this means that
 * there is one marble between the marble that was just placed and the current
 * marble.) The marble that was just placed then becomes the current marble.
 *
 * However, if the marble that is about to be placed has a number which
 * is a multiple of 23, something entirely different happens. First, the
 * current player keeps the marble they would have placed, adding it to
 * their score. In addition, the marble 7 marbles counter-clockwise from the
 * current marble is removed from the circle and also added to the current
 * player's score. The marble located immediately clockwise of the marble
 * that was removed becomes the new current marble.
 *
 * For example, suppose there are 9 players. After the marble with value 0
 * is placed in the middle, each player (shown in square brackets) takes a
 * turn. The result of each of those turns would produce circles of marbles
 * like this, where clockwise is to the right and the resulting current
 * marble is in parentheses:
 *
 *     [-] (0)
 *     [1]  0 (1)
 *     [2]  0 (2) 1
 *     [3]  0  2  1 (3)
 *     [4]  0 (4) 2  1  3
 *     [5]  0  4  2 (5) 1  3
 *     [6]  0  4  2  5  1 (6) 3
 *     [7]  0  4  2  5  1  6  3 (7)
 *     [8]  0 (8) 4  2  5  1  6  3  7
 *     [9]  0  8  4 (9) 2  5  1  6  3  7
 *     [1]  0  8  4  9  2(10) 5  1  6  3  7
 *     [2]  0  8  4  9  2 10  5(11) 1  6  3  7
 *     [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
 *     [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
 *     [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
 *     [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
 *     [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
 *     [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
 *     [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
 *     [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
 *     [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
 *     [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
 *     [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
 *     [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
 *     [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
 *     [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
 *
 * The goal is to be the player with the highest score after the last marble is
 * used up. Assuming the example above ends after the marble numbered 25, the
 * winning score is 23+9=32 (because player 5 kept marble 23 and removed marble
 * 9, while no other player got any points in this very short example game).
 *
 * Here are a few more examples:
 *
 * - 10 players; last marble is worth 1618 points: high score is 8317
 * - 13 players; last marble is worth 7999 points: high score is 146373
 * - 17 players; last marble is worth 1104 points: high score is 2764
 * - 21 players; last marble is worth 6111 points: high score is 54718
 * - 30 players; last marble is worth 5807 points: high score is 37305
 *
 * What is the winning Elf's score?
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

module IntMap = Map.Make(Int)

type state = {
    players: int;
    round: int;
    current_i: int;
    circle: int list;
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
    let len = List.length circle in
    let n = n mod len in
    if n >= 0
    then n
    else n + len
;;

(** insert item before index i (the inserted item will now have index i) *)
let rec list_insert item i l : 'a list =
    if i <= 0 then item::l else match l with
    | [] -> failwith "index out of range for list"
    | hd::tl -> hd::(list_insert item (i - 1) tl)
;;

let rec list_remove i l : 'a * 'a list =
    match l with
    | [] -> failwith "index out of range for list"
    | hd::tl -> if i = 0 then hd, tl else let i, l = list_remove (i - 1) tl in i, hd::l
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
    current_i;
    circle;
    scores;
} : state =
    let round = round + 1 in
    let current_player = current_player players round in

    if round mod 23 = 0 then
        let current_i = (nth_clockwise (current_i - 7) circle) in
	let removed, circle = list_remove current_i circle in
        let current_i = if current_i >= List.length circle then 0 else current_i in

        let scores = add_score current_player (round + removed) scores in
        { players; round; current_i; circle; scores; }
    else
        let current_i = (nth_clockwise (current_i + 2) circle) in
        let circle = list_insert round current_i circle in
        { players; round; current_i; circle; scores; }
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

let output_board ch s =
    let { circle; current_i; players; round; } = s in
    let circle_str = circle
    |> List.map string_of_int
    |> List.mapi (fun i s -> if i = current_i then "(" ^ s ^ ")" else s)
    |> List.map ((^) "\t")
    |> String.concat ""
    in
    Printf.fprintf ch "[%d]%s\n" (current_player s.players s.round) circle_str;
;;

let () =
    let players, marbles = stdin |> input_line |> parse_ctx in

    (* let marbles = marbles * 100 in (* part 2 *) *)

    let state = {
        players;
        round = 0;
        current_i = 0;
        circle = [0];
        scores = IntMap.empty;
    } in

    Seq.cons state (play_for marbles state)
    (* |> seq_viewer (output_board stderr) (* (slower) *) *)
    |> seq_viewer (fun s -> Printf.eprintf "%d\n" s.round; flush stderr)
    (* |> seq_viewer (fun {scores;} -> output_scores stderr scores) *)
    |> seq_last
    |> (fun s ->
        output_scores stderr s.scores;
        flush stderr;
        high_score s.scores |> Printf.printf "%d\n")
;;
