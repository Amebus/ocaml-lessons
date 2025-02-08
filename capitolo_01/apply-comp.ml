let double = function x -> x * 2;;
let triple = function x -> x * 3;;

let apply(f,x) = f x;;

let comp(f,g) = let h x = f(g x)
                in h;;

(* let comp(f,g) x = f(g x);; *)
                
let six_times = comp(double, triple);;


let sum (f,n,m) = 
  let base = 0
  in let c = m-n
  in let rec sum_in (f,n,m,c,acc) = match c with
      0 -> 0
    | k -> sum_in(f,n+1,m,c-1,acc+f n)
  in sum_in(f,n,m,c,base);;

let rec sum2 (f,n,m) =
  if n>m then 0
  else f(n) + sum(f,n+1,m);;


let k x y = x;;


let sum (m,n) = m + n;;
let plus m n = m+n;;
let sommacento = plus 100;;

(* versione riportata (currificata) di comp *)
let b f g x = f (g x);;
let pair x y = (x,y);;
let lessthen x y = y < x;;

let lessthen10 = lessthen 10