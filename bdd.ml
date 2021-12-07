open Utils.Tools;;
open Utils.Math;;

type leaf = {
  id : int;
  decision : bool;
  mutable luka_word : string;
}
type node = {
  id : int;
  tag : string;
  mutable luka_word : string;
}

type bdd = Empty | Leaf of leaf | Node of bdd * node * bdd

let bdd_create ttable =
  let len = List.length ttable in
  if not (is_power_2 (Int.to_float len)) then
    raise (Invalid_argument "Malformed truth table")
  else
    let height = Float.to_int( Float.log2 (Int.to_float len)) in
    let ref_id = ref 0 in
    let rec bdd_init height ttable =
      ref_increment ref_id;
      let tmp_id = !ref_id in
      match height with
        | 0 -> Leaf ({ id = tmp_id ; decision = List.hd ttable ; luka_word = "" })
        | h ->
          let tt1,tt2 = split ttable in
          Node (bdd_init (height-1) tt1, { id = tmp_id ; tag = "x"^Int.to_string height ; luka_word = "" }, bdd_init (height-1) tt2)
    in
    bdd_init height ttable
;;

let rec bdd_to_truth_table = function
  | Empty -> []
  | Leaf (l) -> [l.decision]
  | Node (bdd1, n, bdd2) -> List.append (bdd_to_truth_table bdd1) (bdd_to_truth_table bdd2)
;;

let rec bdd_nb_nodes = function
  | Empty -> 0
  | Leaf (l) -> 1
  | Node (bdd1, n, bdd2) -> 1 + (bdd_nb_nodes bdd1) + (bdd_nb_nodes bdd2)
;;

let rec bdd_luka = function
  | Empty -> ""
  | Leaf (l) ->
      l.luka_word <- Bool.to_string l.decision;
      l.luka_word
  | Node (bdd1, n, bdd2) ->
      n.luka_word <- n.tag ^ "(" ^ bdd_luka bdd1 ^ ")(" ^ bdd_luka bdd2 ^ ")";
      n.luka_word
;;

module LukaNodeMap = Map.Make(String);;
module LukaLeafMap = Map.Make(String);;

let bdd_luka_compression b =
  let node_map = LukaNodeMap.empty in
  let leaf_map = LukaLeafMap.empty in
  let rec generate_node_map b node_m =
    match b with
    | Empty -> node_m
    | Leaf (l) -> node_m
    | Node (bdd1, n, bdd2) ->
        let node_m = LukaNodeMap.add n.luka_word n node_m in
        let node_m = generate_node_map bdd1 node_m in
        let node_m = generate_node_map bdd2 node_m in
        node_m
  in
  let rec generate_leaf_map b leaf_m =
    match b with
    | Empty -> leaf_m
    | Leaf (l) -> LukaLeafMap.add l.luka_word l leaf_m
    | Node (bdd1, n, bdd2) ->
        let leaf_m = generate_leaf_map bdd1 leaf_m in
        let leaf_m = generate_leaf_map bdd2 leaf_m in
        leaf_m
  in
  let rec scan b node_map leaf_map =
    match b with
    | Empty -> Empty
    | Leaf (l) ->
        let l_ = LukaLeafMap.find l.luka_word leaf_map in
        Leaf ({ id = l_.id ; decision = true ; luka_word = l_.luka_word })
    | Node (bdd1, n, bdd2) ->
        let n_ = LukaNodeMap.find n.luka_word node_map in
        let b1 = scan bdd1 node_map leaf_map in
        let b2 = scan bdd2 node_map leaf_map in
        Node (b1, { id = n_.id ; tag = n_.tag ; luka_word = n_.luka_word }, b2)
  in
  let node_map = generate_node_map b node_map in
  let leaf_map = generate_leaf_map b leaf_map in
  let cb = scan b node_map leaf_map in
  print_string " CARD=";
  print_int (LukaNodeMap.cardinal node_map);
  print_newline ();
  print_int (LukaLeafMap.cardinal leaf_map);
  print_newline ();
  cb
;;

(* 

      match bdd1, bdd2 with
      | Empty, Empty | Empty, _ | _, Empty -> raise (Invalid_argument "Malformed bdd")
      | Leaf (l1), Leaf (l2) ->
          let l_1 = LukaLeafMap.find l1.luka_word leaf_map in
          let l_2 = LukaLeafMap.find l2.luka_word leaf_map in
          (Node (l_1, n, l_2), node_map, leaf_map)
      | Leaf (l), _ | _, Leaf (l) -> raise (Invalid_argument "Malformed bdd")
      | Node (bdd11, n1, bdd12), Node (bdd21, n2, bdd22) ->
          (*if String.equal n1.luka_word n2.luka_word then (Node (Node (bdd11, n1, bdd12), n, Node (bdd11, n1, bdd12)),m)
          else (Node (bdd1, n, bdd2),m)*)
          (*let n_1 = LukaLeafMap.find n1.luka_word node_m in
          let n_2 = LukaLeafMap.find n2.luka_word node_m in*)
          let (b1, node_map, leaf_map) = scan bdd1 node_map leaf_map in
          let (b2, node_map, leaf_map) = scan bdd2 node_map leaf_map in
          (Node (b1, n, b2), node_map, leaf_map)

*)

let rec bdd_printer = function
    | Empty -> ()
    | Leaf (l) -> Format.printf "leaf %d (%B) luka=%s\n" l.id l.decision l.luka_word
    | Node (bdd1, n, bdd2) ->
      Format.printf "%d (%s) luka=%s\n" n.id n.tag n.luka_word;
      Format.printf "0/";
      bdd_printer bdd1;
      Format.printf "\\1";
      bdd_printer bdd2;
;;

let bdd_to_dot b ~file =
  let c = open_out file in
  let fmt = Format.formatter_of_out_channel c in
    Format.fprintf fmt "digraph bdd {@\n";
    let get_dot_node id label =
      Format.fprintf fmt "%d [label=\"%s\"];\n" id label
    in
    let get_dot_branch_0 orig_id target_id branch_bit =
      if branch_bit == 0 then
        Format.fprintf fmt "%d -> %d [label=\"0\",style=\"dashed\"];\n" orig_id target_id
      else
        Format.fprintf fmt "%d -> %d [label=\"1\"];\n" orig_id target_id
    in
    let get_dot_branch orig_id target_bdd branch_bit =
      match target_bdd with
      | Empty -> ()
      | Leaf (l) -> get_dot_branch_0 orig_id l.id branch_bit
      | Node (bdd1, n, bdd2) -> get_dot_branch_0 orig_id n.id branch_bit
    in
    let rec visit bdd =
      match bdd with
      | Empty -> ()
      | Leaf (l) -> get_dot_node l.id l.luka_word
      | Node (bdd1, n, bdd2) ->
          get_dot_node n.id n.tag;
          get_dot_branch n.id bdd1 0;
          get_dot_branch n.id bdd2 1;
          visit bdd1;
          visit bdd2
    in
    visit b;
    Format.fprintf fmt "}@.";
    close_out c

;;