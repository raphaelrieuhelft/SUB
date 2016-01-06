(* The abstract machine *)

open AST

(* The machine is an extension of the Modern SECD.
   It has three components:
   - the code: a list of instructions
   - the environment: a list of values, accessed by position
   - the stack
*)

(* The instruction set *)

type instruction =
  | Iaccess of int
       (* Pushes the [n]-th entry of the environment on the stack *)
  | Iclosure of code
       (* Builds a closure of the given code with the current environment.
          Pushes this closure on the stack. *)
  | Itailapply
       (* Pop an argument value off the stack, then pop a function closure.
          Perform a tail application of the closure to the argument. *)
  | Iapply
       (* Pop an argument value off the stack, then pop a function closure.
          Perform a regular, non-tail application of the closure to
          the argument. *)
  | Ireturn
       (* Terminate the current function, returning the value at
          the top of the stack as the function result. *)
  | Ilet
       (* Pop a value off the stack, add it at the head of the environment. *)
  | Iendlet
       (* Discard the head of the environment. *)
  | Ipush of constant
       (* Push the given number on the stack. *)
  | Iaddint
       (* Pop two integers, push their sum. *)
  | Iaddfloat
       (* Pop two FP numbers, push their sum. *)
  | Ifloatofint
       (* Pop an integer, convert it to FP, push the result. *)
  | Iintoffloat
       (* Pop a FP number, truncate it to an integer, push the result. *)
  | Imaketuple of int
       (* Pop [n] values off the stack: [v_n] then [v_{n-1}] then ... then [v1].
          Build the [n]-tuple [(v_1, ..., v_n)].
          Push this tuple on the stack. *)
  | Ifield of int
       (* Pop a tuple value off the stack.
          Extract the [n]-th field of this tuple.
          Push the value of this field. *)

and code = instruction list

val execute_program: code -> typ -> unit
  (* [execute_program c t] runs the abstract machine on the code [c].
     The machine starts with an empty stack and an empty environment.
     It runs until all instructions in [c] have been executed.
     At this point, the stack should contain a single machine value.
     This value is then printed on standard output as a value of
     SUB type [t].  The type is used to recover the names of the
     labels in records, which have disappeared during compilation
     (of records to tuples) and execution. *)

val print_program: code -> unit
  (* Print a readable representation of the given code on standard output. *)
