(*Torre di Hanoi*)

(* move : string * string -> string *)
let move(x,y) = "Sposto un disco da " ^ x ^ " a " ^ y ^ "\n";;

(* hanoiaux : int * string * string * string -> stirng *)
let rec hanoiaux (n, inizio, fine, aux) = match n with
		0 -> ""
	|	_ -> hanoiaux(n-1, inizio, aux, fine) ^ (move(inizio, fine)) ^ (hanoiaux(n-1,aux,fine,inizio));;

(* nahoi: int -> unit *)
let hanoi n = print_string (hanoiaux(n, "A", "B", "C"))