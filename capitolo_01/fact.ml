let rec fact_m n = match n with
    0 -> 1
  | n -> n * fact_m(n-1);;

let rec fact_i n = if n = 0 then 1
                 else n * fact_i(n-1)

exception FactError;;
let fact n = 
  let s = 1
  in let rec fact_in(n2,acc) = match n2 with
      0 -> acc
    | n -> if n < 0 
      then raise FactError
      else fact_in(n-1,n*acc)
  in fact_in(n,s)

let rec fact_tr_ (n, acc) = match n with
    0 -> acc
  | n -> fact_tr_(n-1,n*acc);;
let fact_tr n = fact_tr_(n, 1);;

let n = fact_tr 3 in n*n;

