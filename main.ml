open Utils.Math;;
open Utils.Tools;;
open Bdd;;

let () =

  print_string "BDD First tests / draft";;
  print_newline ();;

  (* 38 base 2 decomposition *)
  let ttable = truth_table 38 8;;
  truth_table_printer ttable;;
  truth_table_printer_2 ttable;;
  print_newline ();;

  (* Associated BDD *)
  let bdd = bdd_create ttable;;
  bdd_luka bdd;;
  print_int (bdd_nb_nodes bdd);;
  print_newline ();;
  (*bdd_printer bdd;;*)
  (*truth_table_printer (bdd_to_truth_table bdd);;*) (* to find the truth table back *)
  
  let cbdd = bdd_luka_compression bdd;;
  bdd_printer cbdd;;

  let file = "bdd.dot" in
    bdd_to_dot cbdd ~file;
    (*file_display file*)

;;