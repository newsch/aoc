#! /usr/bin/env ocaml
(* Day 8 Part 2: Memory Maneuver
 *
 * USAGE:
 *
 *    ./day08p2.ml < input.txt
 *
 * CONTEXT:
 *
 * The second check is slightly more complicated: you need to find the value
 * of the root node (A in the example above).
 *
 * The value of a node depends on whether it has child nodes.
 *
 * If a node has no child nodes, its value is the sum of its metadata
 * entries. So, the value of node B is 10+11+12=33, and the value of node
 * D is 99.
 *
 * However, if a node does have child nodes, the metadata entries become
 * indexes which refer to those child nodes. A metadata entry of 1 refers
 * to the first child node, 2 to the second, 3 to the third, and so on. The
 * value of this node is the sum of the values of the child nodes referenced
 * by the metadata entries. If a referenced child node does not exist, that
 * reference is skipped. A child node can be referenced multiple time and
 * counts each time it is referenced. A metadata entry of 0 does not refer
 * to any child node.
 *
 * For example, again using the above nodes:
 *
 *- Node C has one metadata entry, 2. Because node C has only one
 *  child node, 2 references a child node which does not exist, and so
 *  the value of node C is 0.
 *- Node A has three metadata entries: 1, 1, and 2. The 1 references
 *  node A's first child node, B, and the 2 references node A's second
 *  child node, C. Because node B has a value of 33 and node C has a
 *  value of 0, the value of node A is 33+33+0=66.
 *
 * So, in this example, the value of the root node is 66.
 *
 * What is the value of the root node?
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

let rec node_value (n: node) : int =
    if Array.length n.children = 0 then
        n.metadata |> Array.fold_left (+) 0
    else
        n.metadata
        |> Array.to_seq
        |> Seq.map (fun i -> i - 1)
        |> Seq.filter (fun i -> (Array.length n.children) > i)
        |> Seq.map (Array.get n.children)
        |> Seq.map node_value
        |> Seq.fold_left (+) 0
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

    root
    |> node_value
    |> Printf.printf "%d\n"
;;
