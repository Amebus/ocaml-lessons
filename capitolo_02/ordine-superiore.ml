
let l1 = [2;4;6;7;8];;
let even x = x mod 2 = 0;;
let odd x = x mod 2 = 1;;

let takewhile p lst =
  let rec aux = function
  | (_,[], acc) -> acc
  | (p, x::xs, acc) -> match p x with
  | true -> aux(p, xs, acc@[x])
  | false -> acc
in aux(p,lst, []);;

takewhile even l1;;
takewhile odd l1;;

let dropwhile p lst =
  let rec aux = function
  | (_, []) -> []
  | (p, x::xs) -> match p x with
  | true -> aux (p, xs)
  | false -> x::xs
in aux (p, lst);;

dropwhile even l1;;
takewhile odd l1;;

let map f lst = 
  let rec aux = function 
  | ([], acc) -> acc
  | (x::xs,acc) -> aux(xs, acc@[f x])
in aux (lst, []);;

map even l1;;
map odd l1;;

let map_double = map (function n -> 2*n);;

map_double l1;;

let map_square = map (function n -> n*n);;

map_square l1;;


let filter p lst =
  let rec aux = function
  | ([],acc) -> acc
  | (x::xs, acc) -> match p x with
  | true -> aux (xs, acc@[x])
  | false -> aux (xs,acc)
in aux(lst, []);;

filter even l1;;
filter odd l1;;


let for_all p lst =
  let rec aux = function
  | [] -> true
  | x::xs -> match p x with
    | false -> false
    | true -> aux xs
in aux lst;;

for_all (function x -> x > 0) l1;;
for_all even l1;;

let exists p lst =
  let rec aux = function
  | [] -> false
  | x::xs -> match p x with
  | true -> true
  | false -> aux xs
in aux lst;;

(* #use "ordine-superiore.ml";; *)