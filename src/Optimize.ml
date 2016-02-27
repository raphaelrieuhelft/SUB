(* Optimization of coercions in an IL expression *)
open AST
open IL


(* Optimization of expressions *)

(* c1(c2(l)) *)

let rec compose_coercions c1 c2 = match c1, c2 with
	| Cid, _ -> c2
	| _, Cid -> c1
	| Cfun(arg1, res1), Cfun(arg2, res2) -> Cfun (compose_coercions arg2 arg1, compose_coercions res1 res2)  (* c1(res2 o f o arg2) = res1 o res2 o f o arg2 o arg1 *)
	| Crecord l1, Crecord l2 -> 
		let f = fun (i1, c1) -> let (i2, c2) = List.nth l2 (i1-1) in (i2, compose_coercions c1 c2) in
		Crecord (List.map f l1)
	|_ -> assert false
			


let rec is_id_record l i = match l with
	| [] -> true
	| (i,Cid)::t -> is_id_record t (i+1)
	| _ -> false
			
let rec optim_coercion (l: lam) (c:coercion) : lam = 
	match l,c with
	| Ltuple l1, Crecord l2 when List.length l1 = List.length l2 && (is_id_record l2 1) 
		-> Lcoerce(Ltuple l1, Cid)
	| _, Cfun(Cid, Cid) -> Lcoerce(l, Cid)
	| Ltuple l1, Crecord l2 -> 
		let small_tuple = List.map (fun (i,c) -> List.nth l1 (i-1)) l2 in
		let rec renumber i = function 
			|[] -> []
			|(_,c) ::t -> (i,c) :: (renumber (i+1) t)
		in
		Lcoerce(Ltuple small_tuple, Crecord (renumber 1 l2))
	| Lcoerce(l1, c1), c2 -> optim (Lcoerce(l1, compose_coercions c2 c1))	
	| Labstr(v,a), Cfun(c1, c2) -> 
		let x = fresh_variable () in
		optim (Labstr(x, Lcoerce(Llet(v, Lcoerce(Lvar x, c1), a), c2)))
	| _ -> Lcoerce(l,c)

and optim (l: lam): lam =
	match l with
	| Lvar v -> Lvar v
	| Labstr(v, l) -> Labstr(v, optim l)
	| Lapp(Lcoerce(f, Cfun(c1, c2)), arg) -> optim (Lcoerce(Lapp(f, Lcoerce(arg, c1)),  c2))
	| Lapp(l1, l2) -> Lapp(optim l1, optim l2)
	| Llet(v, l1, l2) -> Llet(v, optim l1, optim l2)
	| Lconst c -> Lconst c
	| Lunop(op, l1) -> Lunop(op, optim l1)
	| Lbinop(op, l1, l2) -> Lbinop(op, optim l1, optim l2)
	| Ltuple ll -> Ltuple (List.map optim ll)
	| Lfield(Lcoerce(l1, Crecord lc), n) ->
		let (i, c) = List.nth lc (n-1) in
		optim (Lcoerce(Lfield(l1, i), c))
	| Lfield(l, n) -> Lfield(optim l, n)
	| Lcoerce(l,c) -> optim_coercion (optim l) c