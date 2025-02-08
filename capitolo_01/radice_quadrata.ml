(* let epsilon = 0.001;;
let okp(x,a) = abs_float(a -. x *. x) < epsilon;;
let next(x,a) = (a /. x +. x) /. 2.0;;
let rec findroot(a,x)
  = let newval = next(x,a)
    in if okp(newval,a) then newval else findroot(a,newval);;

let sqroot a = findroot(a, 1.0);;
*)

let sqroot a = 
	let epsilon = 0.001
	in let okp x = abs_float(a -. x *. x) < epsilon
	in let next x = (a /. x +. x) /. 2.0
	in let rec findroot x
		=	let newval = next x
			in	if okp newval then newval
					else findroot newval
	in findroot 1.0
