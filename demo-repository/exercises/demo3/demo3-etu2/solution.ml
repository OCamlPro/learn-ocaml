
(* First part : simple functions on integers *)

let sum3 a b c = a+b+c;;

let plus2 a = a+2 ;;

(* Second part : recursive functions on integers *)

let rec sum_0_to_n n =
  if n<0 then failwith "sum_0_to_n : arg < 0 not allowed" else
    match n with
    |0-> 0
    |n -> n+sum_0_to_n (n-1);;

let rec fact n =
  match n with
  | 0 | 1 -> 1
  | n -> n*fact (n-1);;

