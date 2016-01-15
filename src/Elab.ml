(* Type-directed elaboration of SUB expressions into IL expressions *)

open Printf
open AST
open IL

(* Reporting of type errors *)

exception Type_error of string

let type_error msg = raise (Type_error msg)


(* Subtyping check.  If [t1] is a subtype of [t2], return the coercion 
   that transforms values of type [t1] into values of type [t2].
   If [t1] is not a subtype of [t2], raise [Not_subtype] exception. *)

exception Not_subtype


let rec find_index lab l = match l with
	|[] -> raise Not_found
	|(lab1, t)::_ when lab = lab1 -> (1,t)
	|_::l2 -> let (n,t) = find_index lab l2 in (n+1, t)
	

let rec subtype t1 t2 = 
  let rec aux l1 l2 =
	match l2 with
	  |[] -> []
	  |(lab, t2)::l -> 
		  begin
			  try let (k, t1) = find_index lab l1 in (k, (subtype t1 t2))::(aux l1 l)
			  with Not_found -> raise Not_subtype
		  end
  in
  match t1,t2 with
	| _, Top -> Cid
	| _,_ when t1=t2 -> Cid
	| Int, Float -> Cint2float
	| Arrow(s1,s2), Arrow(t1,t2) -> Cfun((subtype t1 s1),(subtype s2 t2))
	| Record l1, Record l2 -> Crecord (aux l1 l2)
	|_-> raise Not_subtype
	
	
(* Elaborate the expression [e] in typing environment [env].
   Return a pair of the principal type and the IL term.
   Raise [Type_error] if the expression is ill-typed. *)

let rec elab env e = 
  let le = 
	match e with
	  | Evar v -> Lvar v
	  | Eabstr (v,t,e) -> let (_,le) = elab (add_typenv v t env) e in Labstr(v,le)
	  | Eapp (f,a) -> 
		begin match elab env f with 
		  |(Arrow(t1,t2), lf) -> let la = check env a t1 in Lapp(lf, la)
		  |(t,_) -> type_error ("Attempting to apply to a non-function of type "^(pretty_typ t))
		end
	  | Elet (v, e1, e2) -> 
		let (t1, le1) = elab env e1 in 
		let (t2, le2) = elab (add_typenv v t1 env) e2 in
		Llet (v, le1, le2)
	  | Econst c -> Lconst c
	  | Eunop (u,e1) -> let (t1, _) = type_of_unop u in let la = check env e1 t1 in Lunop(u, la)
	  | Ebinop (b, e1, e2) -> 
		let (t1, t2, _) = type_of_binop b in 
		let la1 = check env e1 t1 in 
		let la2 = check env e2 t2 in
		Lbinop (b, la1, la2)
	  | Erecord l -> 
		let rec aux = function [] -> [] | (lab,h)::t -> (snd (elab env h))::(aux t)
		in Ltuple (aux l)
	  | Efield (r, lab) -> 
		let (t, le) = elab env r in 
		begin match t with
		  | Record l -> Lfield (le, fst (find_index lab l))
		  | t -> type_error ("Attempting to access label "^lab^" of a non-record of type "^(pretty_typ t))
		end
	  | Econstraint (e, t) -> check env e t
  in (Typing.infer env e, le)


(* Check that expression [e] has type [t] in typing environment [env].
   Return its lambda translation if so.
   Raise [Type_error] if not. *)

and check env e t = 
	let (t1, le) = elab env e in
	let co = subtype t1 t in Lcoerce(le,co)
