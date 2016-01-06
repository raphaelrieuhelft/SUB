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

let rec subtype t1 t2 = raise Not_subtype 

(* Elaborate the expression [e] in typing environment [env].
   Return a pair of the principal type and the IL term.
   Raise [Type_error] if the expression is ill-typed. *)

let rec elab env e = type_error "TODO" 

(* Check that expression [e] has type [t] in typing environment [env].
   Return its lambda translation if so.
   Raise [Type_error] if not. *)

and check env e t = type_error "TODO" 
