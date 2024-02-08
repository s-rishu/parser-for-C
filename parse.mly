/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */

//defining tokens for constants
%token <int> INT 
%token <float> FLOAT 

//defining tokens for vars
%token <string> VAR

//defining tokens for operations
%token PLUS
%token MINUS
%token MUL
%token DIV

%token GREATER_THAN
%token LESS_THAN
%token EQUAL_TO
%token NOT_EQUAL_TO
%token GREATER_THAN_EQUAL_TO
%token LESS_THAN_EQUAL_TO

%token AND
%token OR
%token NOT

%token ASSIGN

//defining tokens for keywords
%token FOR
%token IF
%token ELSE
%token WHILE
%token RETURN

//defining tokens for bracks
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

//defining token for end of line
%token EOF

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }

stmt :
  /* empty */ { (Ast.skip, 0) } 
