open Utils.Math;;
open Utils.Tools;;
open Bdd;;

let () =

  (* Example 1 : bdd from 38 truth table decomposition *)
  let ttable_38 = truth_table 38 8 in
  let bdd_38 = bdd_create ttable_38 in
  let cbdd_38 = bdd_luka_compression bdd_38 in
    truth_table_printer ttable_38;
    truth_table_printer_2 ttable_38;
    print_newline ();
    print_int (bdd_nb_nodes bdd_38);
    print_newline ();
    print_int (bdd_nb_nodes cbdd_38);
    print_newline ();
    let file = "bdd_38.dot" in bdd_to_dot bdd_38 ~file;
    let file = "cbdd_38.dot" in bdd_to_dot cbdd_38 ~file;
  ;;

  (* Example 2 : bdd from big integer truth table decomposition *)
  let ttable_ = truth_table 1234567890000000000 256 in
  let bdd_ = bdd_create ttable_ in
  let cbdd_ = bdd_luka_compression bdd_ in
    truth_table_printer_2 ttable_;
    print_newline ();
    print_int (bdd_nb_nodes bdd_);
    print_newline ();
    print_int (bdd_nb_nodes cbdd_);
    print_newline ();
    let file = "bdd_.dot" in bdd_to_dot bdd_ ~file;
    let file = "cbdd_.dot" in bdd_to_dot cbdd_ ~file;
  ;;

  (* scalability complexity
      - 4 variables > 2^(2^4)=2^16=65.536 combinaisons (OK)
      - 5 variables > 2^32=4.294.967.296 combinaisons (KO!)
  *)
  let t = Sys.time() in
  let stat_array = robdd_benchmark 4 in
  Format.printf "Execution time: %fs\n" (Sys.time() -. t);
  for i = 0 to (Array.length stat_array)-1 do
    Format.printf "%d nodes >> count %d\n" i stat_array.(i)
  done
  ;;

;;