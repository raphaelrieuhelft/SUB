(* The intermediate language Lambda *)

open AST

(* Coercions describe the changes of data representations that must
   be performed when an expression is viewed with a supertype of
   its original type.  *)

type coercion =
  | Cid
      (* The identity coercion: no change of representation *)
  | Cint2float
      (* Convert an integer to a floating-point number *)
  | Cfun of coercion * coercion
      (* [Cfun(c1, c2)] transforms a function [f] by composing it
         with the coercions [c1] and [c2], as in [c2 o f o c1].
         In other words, this coercion transforms a function [f]
         into another function that first coerces its argument
         as described by [c1], then calls [f], then coerces
         the result of [f] as described by [c2]. *)
  | Crecord of (int * coercion) list
      (* [Crecord [(i_1, c_1); ...; (i_n, c_n)]] transforms a
         tuple [r] representing a record, as follows.
         The resulting tuple has [n] fields.
         Field number [k] is obtained by taking field number [i_k]
         in [r], then coercing it as described by [c_k]. *)

(* Expressions of the Lambda language *)

type lam =
  | Lvar of variable              (* Variables *)
  | Labstr of variable * lam      (* Function abstraction *)
  | Lapp of lam * lam             (* Function application *)
  | Llet of variable * lam * lam  (* "let" binding *)
  | Lconst of constant            (* Constants *)
  | Lunop of unop * lam           (* Unary arithmetic "op e" *)
  | Lbinop of binop * lam * lam   (* Binary arithmetic "e1 op e2" *)
  | Ltuple of lam list
       (* Construct a tuple.  [Ltuple [e_1; ...; e_n]] produces
          a [n]-tuple with [e_1] as first field, [e_2] as second field, etc. *)
  | Lfield of lam * int
       (* Access a field of a tuple.  [Lfield(e,n)] returns the
          [n]-th field of the tuple [e].  The first field corresponds
          to [n = 1]. *)
  | Lcoerce of lam * coercion
       (* Apply a coercion.  [Lcoerce(e, c)] transforms [e] as
          described by the coercion [c]. *)

(* Utility functions *)

val print_lam: lam -> unit
  (* Pretty-print the given expression on standard output. *)
