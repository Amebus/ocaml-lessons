let l1 = [3;6;7;9];;

exception NullList;;

let hd l = match l with
| x::_ -> x
| _ -> raise NullList;;

let tl l = match l with
| _::x -> x
| _ -> raise NullList;;

let null lst = lst = [];;

let concat l1 l2 = l1 @ l2;;

let list_len l = 
  let rec aux l acc = match l with
  | [] -> acc
  | x::xs -> aux xs acc+1
in aux l 0;;

list_len l1;;

let square n = n*n;;
let square_list l =
  let rec aux l acc = match l with
  | [] -> acc
  | x::xs -> aux xs acc@[square x ]
in aux l [] ;;

square_list l1;;

let upto m n  = 
  let rec aux m v acc = match v with
  | 0 -> acc
  | v -> aux (m+1) (v-1) acc@[m+1]
in aux m (n-m) [];;

(* upto (10,2);; *)
upto 10 20;;

let mem (y,lst) = 
  let rec mem_r (y, lst, acc) = match (lst,acc) with
  | (_, true) -> true
  | ([], _) -> false
  | (x::xs, _) -> mem_r(y, xs, x=y)
in mem_r (y, lst, false);;

mem (6,l1);;
mem (2,l1);;


let remove (v,lst) =
  let to_list (x,y) = if x = y then [] else [y]
in let rec r (v,lst,acc) = match (v,lst) with
  | (_, []) -> acc
  | (v, x::xs) -> r (v, xs, acc @ to_list(v,x))
in r (v,lst,[]);;

remove(7,l1);;
remove(10,l1);;


let reverse lst = 
  let rec reverse_r (lst, acc) = match lst with
  | [] -> acc
  | y::ys -> reverse_r(ys, y::acc)
in reverse_r(lst,[]);;

reverse l1;;

let take (lst, n) = 
  let rec take_r (lst,n,acc) = match (lst,n,acc) with
  | (_,0,acc) -> acc
  | ([], _, acc) -> acc
  | (x::xs,n,acc) -> take_r (xs,n-1,acc@[x])
in take_r(lst,n,[]);;

take(l1,2);;
take(l1,0);;

let drop (lst, n) =
  let rec drop_r = function
  | (_,0,acc) -> acc
  | ([],_,acc) -> acc
  | (x::xs,n,acc) -> drop_r(xs,n-1,xs)
in drop_r(lst,n,lst);;

drop (l1,0);;
drop (l1,2);;

let flatten lst = 
  let rec flatten_r = function
  | ([], acc) -> acc
  | (x::xs, acc) -> flatten_r(xs, acc@x)
in flatten_r (lst, []);;


flatten([[1;2];[3;4];[5]]);;

(* let rec implode = function
| [] -> ""
| x::xs -> (String.make 1 x) ^ implode xs;; *)

let implode chars = 
  let rec implode_r = function 
  | ([], acc) -> acc
  | (x::xs, acc) -> implode_r(xs, acc ^ (String.make 1 x))
in implode_r(chars, "");;

implode ['a';'b';'c'];;

(* let rec explode str =
  let maxindex = (String.length str) -1 in
  let rec aux n =
    if n > maxindex then []
    else (str.[n])::aux(n+1)
  in aux 0;; *)
let explode str = 
  let s = List.of_seq (String.to_seq str)
in let rec explode_r = function 
  | ([], acc) -> acc
  | (x::xs, acc) -> explode_r (xs, acc@[x])
in explode_r (s, []);;
  

explode "le cose";;



(* #use "./liste.ml";; *)