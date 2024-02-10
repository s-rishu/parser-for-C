(* Lexer for Fish --- TODO *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let alpha=(['a'-'z']|['A'-'Z'])
let integer=digit+
let comment="/*"_*"*/"
let var_name=alpha(alpha|digit|'_')*

(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }

| "for" { FOR }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { Printf.printf ("found return"); RETURN }

| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMICOLON }

| '+' { Printf.printf ("found plus"); PLUS }
| '-' { MINUS }
| '*' { MUL }
| '/' { DIV }

| "&&" { AND }
| "||" { OR }

| ">=" { GTE }
| "<=" { LTE }
| '>' { GT }
| '<' { LT }
| "==" { EQ }
| "!=" { NEQ }

| '!' { NOT }
| '=' { ASSIGN }

| var_name { VAR(Lexing.lexeme lexbuf) }
| integer { NUM(int_of_string(Lexing.lexeme lexbuf)) }

| comment { lexer lexbuf }
| _* { Printf.printf ("found nothing useful"); lexer lexbuf }
