(* The intermediate language Lambda *)

open AST

type coercion =
  | Cid
  | Cint2float
  | Cfun of coercion * coercion
  | Crecord of (int * coercion) list

type lam =
  | Lvar of variable
  | Labstr of variable * lam
  | Lapp of lam * lam
  | Llet of variable * lam * lam
  | Lconst of constant
  | Lunop of unop * lam
  | Lbinop of binop * lam * lam
  | Ltuple of lam list
  | Lfield of lam * int
  | Lcoerce of lam * coercion

(* Pretty-printer *)

open Format

let rec print0 pp = function
  | Lvar v -> fprintf pp "%s" v
  | Lconst(Cint n) -> fprintf pp "%d" n
  | Lconst(Cfloat n) -> fprintf pp "%F" n
  | Ltuple ll ->
      fprintf pp "@[<hov1>[%a]@]" print_tuple (true, ll)
  | Lfield(l, n) ->
      fprintf pp "%a.%d" print0 l n
  | Lcoerce(l, c) ->
      fprintf pp "@[<hov1>(%a >>@ %a)@]" 
                 print2 l print_coercion c
  | l ->
      fprintf pp "@[(%a)" print l

and print1 pp = function
  | Lapp(l1, l2) ->
      fprintf pp "%a@ %a" print1 l1 print0 l2
  | Lunop(op, l) ->
      fprintf pp "%s@ %a"
        (match op with Ointoffloat -> "int" | Ofloatofint -> "float")
        print0 l
  | l -> print0 pp l

and print2 pp = function
  | Lbinop(op, l1, l2) ->
      fprintf pp "@[<hov0>%a@ %s %a@]"
          print1 l1
          (match op with Oaddint -> "+" | Oaddfloat -> "+.")
          print2 l2
  | l -> print1 pp l

and print pp = function
  | Labstr(v, l) ->
      fprintf pp "@[<hov2>fun %s ->@ %a@]" v print l
  | Llet(v, l1, l2) ->
      fprintf pp "@[<hov0>let %s = %a in@ %a@]" v print l1 print l2
  | l -> print2 pp l

and print_tuple pp (first, ll) =
  match ll with
  | [] -> ()
  | l :: ll ->
      if not first then fprintf pp ",@ ";
      print2 pp l; print_tuple pp (false, ll)

and print_coercion pp = function
  | Cid -> fprintf pp "id"
  | Cint2float -> fprintf pp "int2float"
  | Cfun(c1, c2) ->
      fprintf pp "fun(%a,%a)" print_coercion c1 print_coercion c2
  | Crecord rc ->
      fprintf pp "tuple(%a)" print_coercion_tuple (true, rc)

and print_coercion_tuple pp (first, rc) =
  match rc with
  | [] -> ()
  | (pos, c) :: rc ->
      if not first then fprintf pp ",@ ";
      fprintf pp "%d:%a" pos print_coercion c;
      print_coercion_tuple pp (false, rc)

let print_lam l =
  printf "%a@." print l
