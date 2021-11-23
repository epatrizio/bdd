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

let rec bdd_luka = function
  | Empty -> ""
  | Leaf (l) ->
      l.luka_word <- Bool.to_string l.decision;
      l.luka_word
  | Node (bdd1, n, bdd2) ->
      n.luka_word <- n.tag ^ "(" ^ bdd_luka bdd1 ^ ")(" ^ bdd_luka bdd2 ^ ")";
      n.luka_word
;;

(* Prefix *)
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