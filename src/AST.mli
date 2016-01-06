(* Abstract syntax for the source language SUB *)

type variable = string  (* Names of variables *)
type label = string     (* Names of record labels *)

(* Type expressions *)

type typ =
  | Top                 (* The universal "top" type *)
  | Int                 (* The type of integer numbers *)
  | Float               (* The type of floating-point numbers *)
  | Arrow of typ * typ  (* Function types t1 -> t2 *)
  | Record of rectyp    (* Record types { ... lbl : t ... } *)

and rectyp = (label * typ) list

(* Constants and arithmetic operators *)

type constant =
  | Cint of int         (* Integer constant *)
  | Cfloat of float     (* Floating-point constant *)

type unop =
  | Ointoffloat         (* Truncate a float to an integer *)
  | Ofloatofint         (* Convert an integer to a float *)

type binop =
  | Oaddint             (* Integer addition "+" *)
  | Oaddfloat           (* Floating-point addition "+." *)

(* Expressions *)

type expr =
  (* The lambda-calculus *)
  | Evar of variable                    (* Variables *)
  | Eabstr of variable * typ * expr     (* fun (x : t) -> e *)
  | Eapp of expr * expr                 (* Function application *)
  | Elet of variable * expr * expr      (* let x = e1 in e2 *)
  (* Arithmetic *)
  | Econst of constant                  (* Constants *)
  | Eunop of unop * expr                (* Unary arithmetic "op e" *)
  | Ebinop of binop * expr * expr       (* Binary arithmetic "e1 op e2" *)
  (* Records *)
  | Erecord of (label * expr) list      (* { ... lbl = e ... } *)
  | Efield of expr * label              (* e.lbl *)
  (* Type constraint *)
  | Econstraint of expr * typ           (* (e : t) *)

(* Utility functions *)

val pretty_typ: typ -> string
  (* Return a string representing the given type in concrete syntax.
     Useful to produce type error messages, and for debugging. *)

val type_of_constant: constant -> typ
  (* Return the minimal type for the given constant ("int" or "float") *)

val type_of_unop: unop -> typ * typ
  (* Return a pair (type of argument, type of result). *)

val type_of_binop: binop -> typ * typ * typ
  (* Return a triple
       (type of argument 1, type of argument 2, type of result). *)

val fresh_variable: unit -> variable
  (* Produce a fresh name for a variable.  The names are of the form
     "%1", "%2", etc, and cannot be confused with variable names
     present in the source SUB terms.  A different variable name
     is returned at every call. *)

(* Typing environments *)

type typenv
  (* The abstract type of type environments, that is, finite maps
     from variable names to types. *)

val empty_typenv: typenv
  (* The empty environment *)

val add_typenv: variable -> typ -> typenv -> typenv
  (* [add_typenv x t env] returns a type environment that maps 
     variable [x] to type [t], and other variables like [env] does. *)

val lookup_typenv: variable -> typenv -> typ option
  (* [lookup_typenv x env] returns [Some t] if [env] maps 
     variable [x] to type [t].  
     If [x] is not bound in [env], returns [None]. *)

(* Error reporting *)

exception Duplicate_label of string
  (* Exception raised by the parser when it sees ill-formed
     record types or record expressions such as {x = 1; x = 2}. *)
