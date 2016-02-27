(* Compilation of IL (without coercions) to abstract machine code *)

open Printf
open AST
open IL
open Mach

exception Error of string

module StringMap = Map.Make(String)
type debruijnenv = int StringMap.t

let shift_and_add (env : debruijnenv) (v : variable) = 
	StringMap.add  v 1 (StringMap.map (fun n -> n+1) (StringMap.remove v env))

let program (l: lam) : code = 
  let rec compile (l: lam) (env : debruijnenv) =
	match l with 
	 | Lvar v -> [Iaccess (StringMap.find v env)]	(*never fails*)
	 | Labstr (v, a) -> [Iclosure (tail_compile a (shift_and_add env v))] (*try to tail compile*)
	 | Lapp (a, b) -> (compile a env)@(compile b env)@[Iapply]
	 | Llet (v, a, b) -> (compile a env) @ (Ilet :: ((compile b (shift_and_add env v))@[Iendlet]))
	 | Lconst c -> [Ipush c]
	 | Lunop (o, a) -> 
		let code_of_unop = function Ointoffloat -> [Iintoffloat] | Ofloatofint -> [Ifloatofint] in
		(compile a env) @ (code_of_unop o)
	 | Lbinop (o, a, b) ->
		let code_of_binop = function Oaddint -> [Iaddint] | Oaddfloat -> [Iaddfloat] in
		(compile a env) @ (compile b env) @ (code_of_binop o)
	 | Ltuple ll -> (List.fold_right(fun a cl -> (compile a env)@cl) ll [])@[Imaketuple (List.length ll)]
	 | Lfield (a, n) -> (compile a env)@[Ifield n]
	 | Lcoerce _ -> assert false (*coercions are eliminated at expansion*)
	
  and tail_compile (l:lam) (env: debruijnenv) =
	match l with
	 | Llet(v,a,b) -> (compile a env) @ (Ilet :: (tail_compile b (shift_and_add env v))) 
		(*no endlet*)
	 | Lapp(a, b) -> (compile a env)@(compile b env)@[Itailapply] (*we can tail apply*)
	 | a -> (compile l env)@[Ireturn] (*not a tail call*)
	 
  in
  compile l StringMap.empty
