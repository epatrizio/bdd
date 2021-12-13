type leaf = { id : int; decision : bool; }
type node = { id : int; tag : string; mutable luka_word : string; }
type bdd = Leaf of leaf | Node of bdd * node * bdd

val bdd_create : bool list -> bdd
val bdd_to_truth_table : bdd -> bool list
val bdd_nb_nodes : bdd -> int
val bdd_luka_compression : bdd -> bdd
val robdd_benchmark : int -> int array
val bdd_printer : bdd -> unit
val bdd_to_dot : bdd -> file:string -> unit