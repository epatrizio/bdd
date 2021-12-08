module Tools = struct

  let rec first_elts list n =
    match list with
      | [] -> []
      | x :: r -> if n <= 0 then [] else if n == 1 then [x] else x :: first_elts r (n-1)
  ;;

  let rec last_elts list n =
    match list with
      | [] -> []
      | x :: r -> if n == List.length r then r else last_elts r n
  ;;

  let rec split list =
    let len = List.length list in
    let len2 = len / 2 in
      (first_elts list len2, last_elts list (len-len2))
  ;;

  let generic_completion default_value list n =
    let len = List.length list in
    let completed_list = first_elts list n in
      if n <= len then
        completed_list
      else
        let tmp_list = List.init (n-len) (fun x -> default_value) in
          List.append completed_list tmp_list
  ;;

  let false_completion = generic_completion false;;

  let rec truth_table_print fmt = function
    | [] -> ()
    | [x] -> Format.fprintf fmt "%B" x
    | x :: r -> Format.fprintf fmt "%B, %a" x truth_table_print r
  ;;

  let truth_table_printer ttable =
    Format.printf "[%a]@." truth_table_print ttable
  ;;

  let bool_print_2 exp b =
    match b with
      | false -> ()
      | true -> Format.printf "2^%d + " exp
  ;;

  let truth_table_printer_2 ttable =
    List.mapi bool_print_2 ttable
  ;;

  let file_display file_name =
    let cmd = Format.sprintf "cat %s" file_name in
      begin try ignore (Sys.command cmd) with _ -> () end;

end;;

module Math = struct

  let increment = fun x -> x + 1;;

  let ref_increment = fun x -> x := !x + 1;;

  let rec power nb ex =
    match ex with
    | 0 -> 1
    | n -> nb * power nb (ex-1)
  ;;

  let power_2 ex = power 2 ex;;

  let is_power_2 elt = Float.is_integer (Float.log2 elt);;

  let euclidean_division divider dividend =
    if divider = 0 then raise Division_by_zero else
    (dividend / divider, dividend mod divider);;

  let euclidean_division_2 = euclidean_division 2;;

  let rec decomposition elt =
    match elt with
      | 0 -> []
      | i ->
        let (q,r) = euclidean_division_2 i in
          if r == 0 then false :: decomposition q
          else true :: decomposition q
  ;;

  let truth_table elt n = Tools.false_completion (decomposition elt) n;;

end;;
