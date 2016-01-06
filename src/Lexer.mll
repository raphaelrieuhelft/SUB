(* An ocamllex lexer for the SUB language *)

{
open Printf
open Parser

exception Error of string
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let ident =
  ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let int_literal =
  '-'? ['0'-'9'] ['0'-'9' '_']*
let float_literal =
  '-'?
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
  | newline     { Lexing.new_line lexbuf; token lexbuf }
  | blank +     { token lexbuf }
  | "(*"        { comment lexbuf; token lexbuf }
  | ":"         { COLON }
  | "."         { DOT }
  | eof         { EOF }
  | "="         { EQUAL }
  | "float"     { FLOAT }
  | "fun"       { FUN }
  | "in"        { IN }
  | "int"       { INT }
  | "{"         { LBRACE }
  | "let"       { LET }
  | "("         { LPAREN }
  | "->"        { MINUSGREATER }
  | "+."        { PLUSDOT }
  | "+"         { PLUS }
  | "}"         { RBRACE }
  | ")"         { RPAREN }
  | ";"         { SEMI }
  | ";;"        { SEMISEMI }
  | "T"         { TOP }
  | ident as s  { IDENT s }
  | int_literal as s { INTCONST (int_of_string s) }
  | float_literal as s { FLOATCONST (float_of_string s) }
  | _ as c      { raise (Error (sprintf "illegal character %C" c)) }

and comment = parse
  | "*)"        { () }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { raise (Error "unterminated comment") }
  | _           { comment lexbuf }
