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
let decimal=digit+('.'digit+)?
let comment="/*".*"*/"
let var_name=alpha(alpha|digit)*

(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }

| "for" { FOR }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { RETURN }

| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMICOLON }

| '+' { PLUS }
| '-' { MINUS }
| '*' { MUL}
| '/' { DIV }

| "&&" { AND }
| "||" { OR }

| ">=" { GREATER_THAN_EQUAL_TO }
| "<=" { LESS_THAN_EQUAL_TO }
| '>' { GREATER_THAN }
| '<' { LESS_THAN }
| "==" { EQUAL_TO }
| "!=" { NOT_EQUAL_TO }

| '!' { NOT }
| '=' { ASSIGN }

| var_name { VAR(Lexing.lexeme lexbuf) }
| decimal { NUM(float_of_string(Lexing.lexeme lexbuf)) }

| comment { lexer lexbuf }
| .* {}
