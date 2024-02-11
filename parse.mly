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
%type <Ast.stmt> stmt, stmt_grp
// %type <Ast.exp> exp
// %type <Ast.rstmt> if_else, while, for


/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */

//defining tokens for constants
%token <int> NUM

//defining tokens for vars
%token <string> VAR

//defining tokens for operations
%token PLUS
%token MINUS
%token MUL
%token DIV

%token GT
%token LT
%token EQ
%token NEQ
%token GTE
%token LTE

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
%token SEMICOLON

//defining token for end of line
%token EOF
%token COMMENT

//defining precedence and associativity
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left MUL DIV

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt_grp EOF { $1 }

stmt_grp: 
  | stmt { ($1) }
  | stmt stmt_grp { (Ast.Seq($1, $2), 0) }

stmt : 
  | LBRACE stmt_grp RBRACE { $2 }
  | exp SEMICOLON { (Ast.Exp($1), 0) }
  | RETURN exp SEMICOLON {(Ast.Return($2), 1) }
  | if_stat {($1, 0)}
  | if_else {($1, 0)}
  | while_loop {($1, 0)}
  | for_loop {($1, 0)}
  // | COMMENT { Printf.printf ("parse: found comment"); (Ast.skip, 0) }


if_only: IF exp stmt { ($2, $3) }
if_stat: if_only { Ast.If(fst $1, snd $1, (Ast.skip, 0)) }
if_else: if_stat ELSE stmt { 
  match $1 with
  | Ast.If(fst, snd, _) -> Ast.If(fst, snd, $3)
 }
while_loop: WHILE exp stmt { Ast.While($2,$3) }
for_loop: FOR LPAREN exp SEMICOLON exp SEMICOLON exp RPAREN stmt { Ast.For($3,$5,$7,$9) }

exp:
  | NUM { (Ast.Int($1), 2) }
  | VAR { (Ast.Var($1), 0) }
  | exp PLUS exp { (Ast.Binop($1,Ast.Plus,$3), 0) }
  | exp MINUS exp { (Ast.Binop($1,Ast.Minus,$3), 0) }
  | exp MUL exp { (Ast.Binop($1,Ast.Times,$3), 0) }
  | exp DIV exp { (Ast.Binop($1,Ast.Div,$3), 0) }
  | exp LT exp { (Ast.Binop($1,Ast.Lt,$3), 0) }
  | exp LTE exp { (Ast.Binop($1,Ast.Lte,$3), 0) }
  | exp GT exp { (Ast.Binop($1,Ast.Gt,$3), 0) }
  | exp GTE exp { (Ast.Binop($1,Ast.Gte,$3), 0) }
  | exp EQ exp { (Ast.Binop($1,Ast.Eq,$3), 0) }
  | exp NEQ exp { (Ast.Binop($1,Ast.Neq,$3), 0) } 
  | exp AND exp { (Ast.And($1,$3), 0) } 
  | exp OR exp { (Ast.Or($1,$3), 0) }
  | NOT exp { (Ast.Not($2), 0) } 
  | MINUS exp { (Ast.Binop((Ast.Int(0), 0),Ast.Minus,$2), 0) }
  | LPAREN exp RPAREN { $2 }
  | VAR ASSIGN exp { (Ast.Assign($1, $3), 0) }

%%