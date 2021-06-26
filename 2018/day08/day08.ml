#! /usr/bin/env ocaml
(* Day 8: Memory Maneuver
 *
 * USAGE:
 *
 *    ./day08.ml < input.txt
 *
 * CONTEXT:
 *
 * The sleigh is much easier to pull than you'd expect for something its
 * weight. Unfortunately, neither you nor the Elves know which way the North
 * Pole is from here.
 *
 * You check your wrist device for anything that might help. It seems to
 * have some kind of navigation system! Activating the navigation system
 * produces more bad news: "Failed to start navigation system. Could not
 * read software license file."
 *
 * The navigation system's license file consists of a list of numbers (your
 * puzzle input). The numbers define a data structure which, when processed,
 * produces some kind of tree that can be used to calculate the license number.
 *
 * The tree is made up of nodes; a single, outermost node forms the tree's
 * root, and it contains all other nodes in the tree (or contains nodes that
 * contain nodes, and so on).
 *
 * Specifically, a node consists of:
 *
 * - A header, which is always exactly two numbers:
 *     - The quantity of child nodes.
 *     - The quantity of metadata entries.
 * - Zero or more child nodes (as specified in the header).
 * - One or more metadata entries (as specified in the header).
 *
 * Each child node is itself a node that has its own header, child nodes,
 * and metadata. For example:
 *
 *     2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
 *     A----------------------------------
 *         B----------- C-----------
 *                          D-----
 *
 * In this example, each node of the tree is also marked with an underline
 * starting with a letter for easier identification. In it, there are
 * four nodes:
 *
 * - A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
 * - B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
 * - C, which has 1 child node (D) and 1 metadata entry (2).
 * - D, which has 0 child nodes and 1 metadata entry (99).
 *
 * The first check done on the license file is to simply add up all of the
 * metadata entries. In this example, that sum is 1+1+2+10+11+12+2+99=138.
 *
 * What is the sum of all metadata entries?
 *)

type node = {
    num: int;
    children: node array;
    metadata: int array;
}

let read_until ch c : string =
    let rec read_until': char Seq.t = fun () ->
        try
            let c' = input_char ch in
            if c' = c then
                Nil
            else
                Cons (c', read_until')
        with End_of_file -> Nil
    in

    read_until'
    |> String.of_seq
;;

let rec do_n d n : 'a Seq.t = fun () ->
    if n <= 0 then Nil else
    Cons (d(), (do_n d (n - 1)))
;;

let parse_tree ch : node =
    let byte = ref 1 in
    let num = ref 0 in

    let get_int () : int =
        let i = read_until ch ' ' in

        (* Printf.eprintf "%d: %S\n" !byte i; *)
        byte := !byte + (String.length i) + 1;

        try
            i |> String.trim |> int_of_string
        with Failure f -> Printf.eprintf "%s: %S@%d" f i !byte; exit 1;
    in


    let rec parse_tree' () =
        num := !num + 1;

        let num_children = get_int()
        and num_metadata = get_int()
        in

        (* Printf.eprintf "Node %d: %d, %d\n" !num num_children num_metadata; *)

        let children = do_n parse_tree' num_children |> Array.of_seq in
        let metadata = do_n get_int num_metadata |> Array.of_seq in

        {
            num = !num;
            children;
            metadata;
        }
    in
    parse_tree' ()
;;

let rec walk_tree (root: node): node Seq.t =
    Seq.cons
        root
        (root.children |> Array.to_seq |> Seq.flat_map walk_tree)
;;


let output_node ch n =
    let print_metadata m : string =
        m |> Array.to_seq |> Seq.map string_of_int |> Seq.map (fun s -> (^) s ";") |> Seq.fold_left (^) ""
    in
    Printf.fprintf ch "Node %d: %d, %d [%s]\n" n.num (Array.length n.children) (Array.length n.metadata) (print_metadata n.metadata)
;;

let () =
    (* recursive parsing and hope it doesn't blow the stack *)
    let root = parse_tree stdin
    in

    flush stderr;

    walk_tree root
    (* |> Seq.iter (output_node stderr) *)
    |> Seq.flat_map (fun n -> n.metadata |> Array.to_seq)
    |> Seq.fold_left (+) 0
    |> Printf.printf "%d\n"
;;
