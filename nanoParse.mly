%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token TRUE FALSE
%token <string> Id
%token EOF
%token LET REC EQ IN FUN
%token ARROW
%token IF THEN ELSE
%token PLUS MINUS
%token MUL DIV
%token LT LE NE 
%token AND OR
%token LPAREN RPAREN
%token LBRAC RBRAC
%token COLONCOLON SEMI

%nonassoc LET IF
%left OR
%left AND
%left EQ EW LT LE
%right COLONCOLON 
%left PLUS MINUS
%left FUN APP

%start exp
%type <Nano.expr> exp

%%

exp:
  | LET Id EQ exp IN exp	{ Let($2, $4, $6) }
  | LET REC Id EQ exp IN exp    { Letrec($3, $5, $7) }
  | FUN Id ARROW exp            { Fun($2, $4) }
  | IF exp THEN exp ELSE exp    { If($2, $4, $6) }
  | operator                    { $1 }

operator:       
  | operator OR operator        { Bin($1, Or, $3) }
  | operator AND operator       { Bin($1, And, $3) }
  | operator EQ contain         { Bin($1, Eq, $3) }
  | operator NE contain         { Bin($1, Ne, $3) }
  | operator LT contain         { Bin($1, Lt, $3) }
  | operator LE contain         { Bin($1, Le, $3) }
  | contain                     { $1 }

contain:
  | math COLONCOLON contain     { Bin ($1, Cons, $3) }
  | math SEMI contain           { Bin ($1, Cons, $3) }
  | LBRAC contain	        { $2 }
  | contain RBRAC               { Bin ($1, Cons, NilExpr) }
  | math                        { $1 }

math:
  | math PLUS math              { Bin($1, Plus, $3) }
  | math MINUS math             { Bin($1, Minus, $3) }
  | math MUL funfun             { Bin($1, Mul, $3) }
  | math DIV funfun             { Bin($1, Div, $3) }
  | funfun                      { $1 }

funfun:
  | funfun data                 { App($1, $2) }
  | data                        { $1 }

data:
  | Num                         { Const ($1) }
  | Id                          { Var ($1) }
  | TRUE                        { True }
  | FALSE                       { False }
  | LPAREN exp RPAREN           { $2 }
  | LBRAC RBRAC			{ NilExpr }
