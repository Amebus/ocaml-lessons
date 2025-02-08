

(* 4 *)

let ten_times x = x * 10;;

let abs x = match x with
    0 -> 0
  | n ->  if n < 0 then n * -1
          else n;;

(* 7 *)
let max_of_tree (x,y,z) = 
  let is_x = x > y && x > z
  in let is_y = y > x && y > z
  in  if is_x then x 
      else if is_y then y
      else z;;

(* 8 *)
let is_date_ok (d,m) = 
  let month = match m with
    | "gennaio" ->  31
    | "febbraio" -> 28
    | "marzo" -> 31
    | "aprile" -> 30
    | "maggio" -> 31
    | "giugno" -> 30
    | "luglio" -> 31
    | "agosto" -> 31
    | "settembre" -> 30
    | "ottobre" -> 31
    | "novembre" -> 30
    | "dicembre" -> 31
    | _ -> 0
  in match month with
  | 0 -> false
  | n -> d <= n;;

(* is_date_ok (12, "");;
is_date_ok (12, "non un mese");;
is_date_ok (12, "febbraio");;
is_date_ok (29, "febbraio");;
is_date_ok (150, "dicembre");;
is_date_ok (31, "dicembre");; *)

(* 9 *)

exception SumOfError;;

let rec sumof_r n = match n with
  | 1 -> 1
  | n ->  if n < 1 then raise SumOfError
          else n + sumof_r (n-1);;

let sumof_i n = 
  if n < 1 then raise SumOfError
  else
    let rec sumof (x,acc)= match x with
    | 0 -> acc
    | y -> sumof (y-1,acc+y)
    in sumof(n, 0);;

(* sumof_r 0;; *)
sumof_i 2;;
sumof_i 10;;



(* 10 *)
exception SumBetweenError;;

let rec sumBetween_r (n,m) = 
  if n > m then raise SumBetweenError
  else if n < m then n + sumBetween_r (n+1,m)
  else n;;

let sumBetween_i (n,m) = 
  if n > m then raise SumBetweenError
  else
    let rec sumof (n,m,i,acc)= match i with
    | 1 -> acc
    | i -> sumof (n+1,m,i-1,acc+n)
    in sumof(n, m, m-n, n);;

sumBetween_i (4,5);;
sumBetween_i (4,6);;

(* 11 *)

let is_power_of_2 n = 
  let rec is_power_of_2 (n,power) = 
    if power > n then false
    else if n == power then true
    else is_power_of_2 (n, power*2)
  in is_power_of_2 (n, 1);;
  (* let div = n / 2
  in let rec is_power_2_in n = n
  in if ab *)

is_power_of_2 4;;
is_power_of_2 128;;
is_power_of_2 6;;

(* 12 *)

let numero_divisori n = 
  if n == 0 then 0
  else
    let rec nd (n,c,acc) = 
      if c > n then acc
      else
        let resto = n mod c
        in match resto with
        | 0 -> nd(n,c+1,acc+1)
        | _ -> nd(n,c+1,acc)
    in nd(n,1,0)

(* 14 *)

let power (n,p) = match (n,p) with
| (_, 0) -> 1
| (n, p) -> 
  let rec p_in (n,p,acc) = match p with
  | 0 -> acc
  | p -> p_in (n, p-1, acc*n)
  in p_in(n,p,1)

let char_repeat (c,n) = match n with
| 0 -> ""
| n -> let rec cr (c,n,acc) = match n with
      | 0 -> acc
      | n -> cr (c,n-1,acc^c)
      in cr (c,n,"");;

(* 17 *)

let second(x,y) = y;;

(* 18 *)
let swap (x,y) = (y,x);;

(* 
#use "esercizi.ml";;
*)