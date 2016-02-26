(* Optimization of coercions in an IL expression *)

open IL


(* Optimization of expressions *)

let rec optim (l: lam) : lam = 
	match l with
	| Lcoerce(l1, Crecord l2) when List.for_all (function (_, Cid) -> true |_-> false) l2 -> l1
	| _ -> l
