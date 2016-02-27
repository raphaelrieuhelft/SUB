(* Optimization of coercions in an IL expression *)

open IL


(* Optimization of expressions *)

let rec optim (l: lam) : lam = 
	match l with
	| Lcoerce(Ltuple l1, Crecord l2) when List.length l1 = 1 && l2 = [(1, Cid)] -> Lcoerce(Ltuple l1, Cid)
	| Lcoerce(Ltuple l1, Crecord l2) -> 
		let small_tuple = List.map (fun (i,c) -> List.nth l1 (i-1)) l2 in
		let rec renumber i = function 
			|[] -> []
			|(_,c) ::t -> (i,c) :: (renumber (i+1) t)
		in
		Lcoerce(Ltuple small_tuple, Crecord (renumber 1 l2))
	| Lcoerce(Lcoerce(l1, c1), c2) -> 
		begin match c1, c2 with
			|Cid, _ -> Lcoerce(l1, c2)
			|_, Cid -> Lcoerce(l1, c1)
			|_-> l
		end		
	| _ -> l
