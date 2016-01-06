(* Compilation of IL (without coercions) to abstract machine code *)

open Printf
open AST
open IL
open Mach

exception Error of string


let program (l: lam) : code = [] 
