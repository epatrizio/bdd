open Utils.Tools;;
open Utils.Math;;

type leaf = {
  decision : bool;
}
type node = {
  tag : string;
  mutable luka_word : string;
}

type bdd = Empty | Leaf of leaf | Node of bdd * node * bdd

let rec bdd_init height ttable =
  match height with
    | 0 -> Leaf ({ decision = List.hd ttable })
    | h ->
      let tt1,tt2 = split ttable in
      Node (bdd_init (height-1) tt1, { tag = "x"^Int.to_string height ; luka_word = "todo" }, bdd_init (height-1) tt2)
;;

let bdd_create ttable =
  let len = List.length ttable in
  if not (is_power_2 (Int.to_float len)) then
    raise (Invalid_argument "Malformed truth table")
  else
    let height = Float.to_int( Float.log2 (Int.to_float len)) in
    bdd_init height ttable
;;

let rec bdd_to_truth_table = function
    | Empty -> []
    | Leaf (l) -> [l.decision]
    | Node (bdd1, n, bdd2) -> List.append (bdd_to_truth_table bdd1) (bdd_to_truth_table bdd2)
;;

(* TODO *)
let rec bdd_printer = function
    | Empty -> ()
    | Leaf (l) -> Format.printf "leaf %B\n" l.decision
    | Node (bdd1, n, bdd2) ->
      Format.printf "node tag=%s luka=%s" n.tag n.luka_word;
      Format.printf " -> 0 ";
      bdd_printer bdd1;
      Format.printf " -> 1 ";
      bdd_printer bdd2;
;;
