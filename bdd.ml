open Utils.Tools;;
open Utils.Math;;

type leaf = {
  id : int;
  decision : bool;
}
type node = {
  id : int;
  tag : string;
  mutable luka_word : string;
}

type bdd = Leaf of leaf | Node of bdd * node * bdd

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
        | 0 -> Leaf ({ id = tmp_id ; decision = List.hd ttable })
        | h ->
          let tt1,tt2 = split ttable in
          Node (bdd_init (height-1) tt1, { id = tmp_id ; tag = "x"^Int.to_string height ; luka_word = "" }, bdd_init (height-1) tt2)
    in
    bdd_init height ttable
;;

let rec bdd_to_truth_table = function
  | Leaf (l) -> [l.decision]
  | Node (bdd1, n, bdd2) -> List.append (bdd_to_truth_table bdd1) (bdd_to_truth_table bdd2)
;;

let bdd_nb_nodes b =
  let cnt_hash = Hashtbl.create 256 in
  let rec bdd_count_nodes b =
    match b with
    | Leaf (l) ->
        (try Hashtbl.find cnt_hash l.id
        with Not_found -> Hashtbl.add cnt_hash l.id 0; 1)
    | Node (bdd1, n, bdd2) ->
        (try Hashtbl.find cnt_hash n.id
        with Not_found -> Hashtbl.add cnt_hash n.id 0; 1)
        + (bdd_count_nodes bdd1) + (bdd_count_nodes bdd2)
  in
  bdd_count_nodes b
;;

(* Opérator ^ string concat has bad complexity (~n²) *)
let rec bdd_luka_01 = function
  | Leaf (l) -> Bool.to_string l.decision
  | Node (bdd1, n, bdd2) ->
      n.luka_word <- n.tag ^ "(" ^ bdd_luka_01 bdd1 ^ ")(" ^ bdd_luka_01 bdd2 ^ ")";
      n.luka_word
;;

(* String.concat seems better *)
let rec bdd_luka_02 = function
  | Leaf (l) -> Bool.to_string l.decision
  | Node (bdd1, n, bdd2) ->
      n.luka_word <- String.concat "" [n.tag;"(";bdd_luka_02 bdd1;")(";bdd_luka_02 bdd2;")"];
      n.luka_word
;;

(* String.concat seems the most efficient *)
let rec bdd_luka = function
  | Leaf (l) -> Bool.to_string l.decision
  | Node (bdd1, n, bdd2) ->
      let buffer = Buffer.create 128 in
      (Buffer.add_string buffer) n.tag;
      (Buffer.add_string buffer) "(";
      (Buffer.add_string buffer) (bdd_luka bdd1);
      (Buffer.add_string buffer) ")(";
      (Buffer.add_string buffer) (bdd_luka bdd2);
      (Buffer.add_string buffer) ")";
      n.luka_word <- Buffer.contents buffer;
      n.luka_word
;;

let bdd_luka_compression b =
  let bdd_hash = Hashtbl.create (bdd_nb_nodes b) in
  let rec bdd_scan b =
    match b with
    | Leaf (l) ->
        (try Hashtbl.find bdd_hash (Bool.to_string l.decision)
        with Not_found ->
          let l_ = Leaf ({ id = l.id ; decision = l.decision }) in
          Hashtbl.add bdd_hash (Bool.to_string l.decision) l_;
          l_
        )
    | Node (bdd1, n, bdd2) ->
        (try Hashtbl.find bdd_hash n.luka_word
        with Not_found ->
          let b1 = bdd_scan bdd1 in
          let b2 = bdd_scan bdd2 in
          if b1 == b2 then
            b1
          else
            let n_ = Node (b1, { id = n.id ; tag = n.tag ; luka_word = "" }, b2) in
            Hashtbl.add bdd_hash n.luka_word n_;
            n_
        )
  in
  bdd_luka b;
  bdd_scan b
;;

let robdd_benchmark_full_distrib nb_vars =
  let tt_size = power_2 nb_vars in
  let bdd_n_nodes = (power_2 (nb_vars+1))-1 in
  let stat_array = Array.make bdd_n_nodes 0 in
  for i = 0 to (power_2 (tt_size-1))-1 do (* tt_size-1 (/2) > truth table symmetry form, full combinaisons not necessary *)
    let ttable = truth_table i tt_size in
    let bdd = bdd_create ttable in
    let cbdd = bdd_luka_compression bdd in
    let nb_nodes = bdd_nb_nodes cbdd in
    stat_array.(nb_nodes) <- (stat_array.(nb_nodes))+2 (* /2 cf. comment > *2 *)
  done;
  stat_array
;;

let robdd_benchmark_random_distrib nb_vars =
  let tt_size = power_2 nb_vars in
  let nb_combinaisons = power_2 tt_size in
  let bdd_n_nodes = (power_2 (nb_vars+1))-1 in
  let stat_array = Array.make bdd_n_nodes 0 in
  for i = 0 to 500000 do
    let n = Int64.to_int(Random.int64 (Int64.of_int nb_combinaisons)) in
    let ttable = truth_table n tt_size in
    let bdd = bdd_create ttable in
    let cbdd = bdd_luka_compression bdd in
    let nb_nodes = bdd_nb_nodes cbdd in
    stat_array.(nb_nodes) <- (stat_array.(nb_nodes))+1
  done;
  stat_array
;;

let rec bdd_printer = function
  | Leaf (l) -> Format.printf "leaf %d (%B)\n" l.id l.decision
  | Node (bdd1, n, bdd2) ->
    Format.printf "%d (%s) luka=%s\n" n.id n.tag n.luka_word;
    Format.printf "0/";
    bdd_printer bdd1;
    Format.printf "\\1";
    bdd_printer bdd2;
;;

let bdd_to_dot b ~file =
  let c = open_out file in
  let cnt_hash = Hashtbl.create 256 in
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
      | Leaf (l) -> get_dot_branch_0 orig_id l.id branch_bit
      | Node (bdd1, n, bdd2) -> get_dot_branch_0 orig_id n.id branch_bit
    in
    let rec visit bdd =
      match bdd with
      | Leaf (l) ->
          (try Hashtbl.find cnt_hash l.id
          with Not_found ->
            Hashtbl.add cnt_hash l.id (print_string ""); get_dot_node l.id (Bool.to_string l.decision))
      | Node (bdd1, n, bdd2) ->
          (try Hashtbl.find cnt_hash n.id
          with Not_found ->
            Hashtbl.add cnt_hash n.id (print_string "");
            get_dot_node n.id n.tag;
            get_dot_branch n.id bdd1 0;
            get_dot_branch n.id bdd2 1);
          visit bdd1;
          visit bdd2
    in
    visit b;
    Format.fprintf fmt "}@.";
    close_out c
;;
