(* Type-checking for the SUB language *)

open Printf
open AST

(* Reporting of type errors *)

exception Type_error of string

let type_error msg = raise (Type_error msg)

(* Subtyping check *)

let rec subtype t1 t2 = 
  let rec aux l1 l2 =
	match l2 with
	|[] -> true
	|(lab, t2)::l -> 
		begin 
			try let t1 = List.assoc lab l1 in if subtype t1 t2 then aux (List.remove_assoc lab l1) l else false
			with Not_found -> false
		end
  in
  match (t1,t2) with
	| _,Top -> true
	| x,y when x=y -> true
	| Int, Float -> true
	| Arrow(s1,s2), Arrow(t1,t2) -> (subtype t1 s1)&&(subtype s2 t2)
	| Record l1, Record l2 -> aux l1 l2
	|_-> false
	
	
	
(* Infer the principal type for expression [e] in typing environment [env].
   Raise [Type_error] if the expression is ill-typed. *)

let rec infer env e =
  match e with
  (* The lambda-calculus *)
  | Evar v ->
      begin match lookup_typenv v env with
      | Some t -> t
      | None -> type_error (sprintf "unbound variable %s" v)
      end
  | Eabstr (v, t1, e) -> let t2 = infer (add_typenv v t1 env) e in Arrow(t1,t2)
  | Eapp (e1, e2) ->
	  begin match infer env e1 with
	  | Arrow(t1,t2) -> check env e2 t1; t2
	  | t -> type_error ("attempting to apply to a non-function of type "^(pretty_typ t))
	  end
  | Elet (v, e1, e2) ->
	  let t1 = infer env e1 in infer (add_typenv v t1 env) e2
  (* Arithmetic *)
  | Econst c ->
      type_of_constant c
  | Eunop(op, e1) ->
      let (targ, tres) = type_of_unop op in
      check env e1 targ;
      tres
  | Ebinop(op, e1, e2) ->
      let (targ1, targ2, tres) = type_of_binop op in
      check env e1 targ1;
      check env e2 targ2;
      tres
  (* Records *)
  | Erecord l -> 
	  begin match l with 
	  |[] -> Record []
	  |(lab,e)::t -> 
		begin match infer env (Erecord t) with 
		| Record l ->
			if List.mem_assoc lab l 
			then type_error ("label "^lab^" appears twice in this record")
			else Record ((lab, infer env e)::l) 
		| _-> assert false
		end
	  end
  | Efield (e,lab) -> 
	  begin match infer env e with 
	  |Record l -> (try List.assoc lab l with Not_found -> raise (Type_error ("label "^lab^" not present in this record")))
	  |t -> type_error ("attempting to type label "^lab^" of a non-record of type "^(pretty_typ t))
	  end
  (* Type constraint *)
  | Econstraint(e, t) ->
      check env e t; t

(* Check that expression [e] has type [t] in typing environment [env].
   Return [()] if true.  Raise [Type_error] if not. *)

and check env e t =
  let t1 = infer env e in
  if not (subtype t1 t) then
    type_error
      (sprintf "expected type %s, got %s" (pretty_typ t) (pretty_typ t1))
