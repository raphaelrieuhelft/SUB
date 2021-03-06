(* Expand coercions within IL expressions.  The result is an IL expression
   without coercions, suitable for compilation to abstract machine code. *)

open AST
open IL


(* Apply a coercion [c] to the expression [l].  The result is a 
   coercion-free Lambda expression. *)

let append_tuple (l: lam) = function Ltuple l2 -> Ltuple (l::l2) |_-> assert false 

let rec apply_coercion (c: coercion) (l: lam) : lam = 
  match c with 
  | Cid -> l
  | Cint2float -> Lunop(Ofloatofint, l)
  | Cfun(c1, c2) ->
	let x = fresh_variable () in
	Labstr(x, apply_coercion c2 (Lapp (l, apply_coercion c1 (Lvar x))))
  | Crecord l1 ->
	let x = fresh_variable () in
	Llet(x, l, Ltuple (List.map (fun (i,c) -> apply_coercion c (Lfield((Lvar x),i)))  l1))
(* Recursively expand the coercions by applying them. *)

let rec expand (l: lam): lam =
  match l with
  | Lvar v -> Lvar v
  | Labstr(v, l) -> Labstr(v, expand l)
  | Lapp(l1, l2) -> Lapp(expand l1, expand l2)
  | Llet(v, l1, l2) -> Llet(v, expand l1, expand l2)
  | Lconst c -> Lconst c
  | Lunop(op, l1) -> Lunop(op, expand l1)
  | Lbinop(op, l1, l2) -> Lbinop(op, expand l1, expand l2)
  | Ltuple ll -> Ltuple (List.map expand ll)
  | Lfield(l, n) -> Lfield(expand l, n)
  | Lcoerce(l, c) -> apply_coercion c (expand l)
