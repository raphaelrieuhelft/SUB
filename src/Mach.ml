(* The abstract machine *)

open Printf
open AST

type instruction =
  | Iaccess of int
  | Iclosure of code
  | Itailapply
  | Iapply
  | Ireturn
  | Ilet
  | Iendlet
  | Ipush of constant
  | Iaddint
  | Iaddfloat
  | Ifloatofint
  | Iintoffloat
  | Imaketuple of int
  | Ifield of int

and code = instruction list

(* A pretty-printer for machine code. *)

let print_indent depth =
  for i = 1 to depth do print_string "    " done

let rec print_instr indent i =
  print_indent indent;
  match i with
  | Iaccess n -> printf "access(%d)\n" n
  | Iclosure c ->
      printf "closure {\n";
      print_code (indent + 1) c;
      print_indent indent; printf "}\n";
  | Itailapply -> printf "tailapply\n"
  | Iapply -> printf "apply\n"
  | Ireturn -> printf "return\n"
  | Ilet -> printf "let\n"
  | Iendlet -> printf "endlet\n"
  | Ipush (Cint n) -> printf "push int %d\n" n
  | Ipush (Cfloat n) -> printf "push float %F\n" n
  | Iaddint -> printf "addint\n"
  | Iaddfloat -> printf "addfloat\n"
  | Ifloatofint -> printf "floatofint\n"
  | Iintoffloat -> printf "intoffloat\n"
  | Imaketuple n -> printf "maketuple %d\n" n
  | Ifield n -> printf "field %d\n" n

and print_code indent c =
  List.iter (print_instr indent) c

let print_program prog =
  print_code 1 prog

(* Representation of machine values and environments *)

type value =
  | Number of constant
  | Tuple of value list
  | Closure of code * environment

and environment = value list

(* Representation of the stack. *)

type stack_slot =
  | Val of value
  | Return_frame of code * environment 

type stack = stack_slot list

(* Exception raised when the machine gets stuck. *)
exception MachineError of string

(* Extract the [n] top entries of the stack. *)
let split_stack n stack =
  let rec split n stack args =
    match n, stack with
    | 0, _ -> (args, stack)
    | _, Val v1 :: s' -> split (n - 1) s' (v1 :: args)
    | _, _ -> raise (MachineError "wrong stack for Imaketuple")
  in split n stack []

(* The transition function for the machine.  Takes the current state
   as argument, returns the next state as result. *)

let transition = function
  | (Iaccess n :: c, e, s) ->
      let vn =
        try List.nth e (n-1)
        with Failure _ -> raise (MachineError "unbound variable") in
      (c, e, Val vn :: s)
  | (Iclosure c' :: c, e, s) ->
      let clos = Closure(c', e) in
      (c, e, Val clos :: s)
  | (Itailapply :: c, e, Val v :: Val (Closure(c', e')) :: s) ->
      (c', v :: e', s)
  | (Iapply :: c, e, Val v :: Val (Closure(c', e')) :: s) ->
      (c', v :: e', Return_frame(c, e) :: s)
  | (Ireturn :: c, e, Val v :: Return_frame(c', e') :: s) ->
      (c', e', Val v :: s)
  | (Ilet :: c, e, Val v :: s) ->
      (c, v :: e, s)
  | (Iendlet :: c, v :: e, s) ->
      (c, e, s)
  | (Ipush cst :: c, e, s) ->
      (c, e, Val (Number cst) :: s)
  | (Iaddint :: c, e, Val (Number (Cint n2)) :: Val (Number (Cint n1)) :: s) ->
      (c, e, Val (Number (Cint (n1 + n2))) :: s)
  | (Iaddfloat :: c, e, Val (Number (Cfloat n2)) :: Val (Number (Cfloat n1)) :: s) ->
      (c, e, Val (Number (Cfloat (n1 +. n2))) :: s)
  | (Ifloatofint :: c, e, Val (Number (Cint n1)) :: s) ->
      (c, e, Val (Number (Cfloat (float_of_int n1))) :: s)
  | (Iintoffloat :: c, e, Val (Number (Cfloat n1)) :: s) ->
      (c, e, Val (Number (Cint (int_of_float n1))) :: s)
  | (Imaketuple n :: c, e, s) ->
      let (vl, s') = split_stack n s in
      (c, e, Val (Tuple vl) :: s')
  | (Ifield n :: c, e, Val (Tuple vl) :: s) ->
      let vn =
        try List.nth vl (n - 1)
        with Failure _ -> raise (MachineError "Ifield failure") in
      (c, e, Val vn :: s)
  | (_, _, _) ->
      raise (MachineError "no transition is possible")

(* Repeat transitions until a final state is reached. *)

let rec exec state =
  match state with
  | ([], e, Val v :: []) -> v
  | _ -> exec (transition state)

(* Print a machine value [v] on standard output.  The value is printed
   according to SUB type [t]. *)

let rec print_value v t =
  match (v, t) with
  | (_, Top) -> printf "_"
  | (Number (Cint n), Int) -> printf "%d" n
  | (_, Int) -> printf "<bad int value>"
  | (Number (Cfloat n), Float) -> printf "%F" n
  | (_, Float) -> printf "<bad float value>"
  | (Closure _, Arrow(t1, t2)) -> printf "<fun>"
  | (_, Arrow(t1, t2)) -> printf "<bad function value>"
  | (Tuple vl, Record rt) ->
      printf "{"; print_record_value true vl rt; printf "}"
  | (_, Record rt) -> printf "<bad record value>"

and print_record_value first vl rt =
  match (vl, rt) with
  | ([], []) -> ()
  | ([], _ ) -> printf "<not enough values>"
  | (_ , []) -> printf "<too many values>"
  | (v :: vl, (lbl, t) :: rt) ->
      if not first then printf "; ";
      printf "%s = " lbl; print_value v t;
      print_record_value false vl rt

(* Execute a code and print its result value. *)
let execute_program code t =
  try
    let v = exec (code, [], []) in
    printf "Value is: "; print_value v t; print_newline()
  with MachineError msg ->
    printf "Machine error: %s\n" msg
