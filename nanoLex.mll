{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  | eof                     { EOF }

  | "true"                  { TRUE }
  | "false"                 { FALSE }

  | [' ' '\n' '\r' '\t']    { token lexbuf } (* skip spaces *)

  | "let"                   { LET }
  | "rec"                   { REC }
  | "="                     { EQ }
  | "in"                    { IN }
  | "fun"                   { FUN }
  | "->"                    { ARROW }

  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }

  | "<"                     { LT }
  | "<="                    { LE }
  | "!="                    { NE }

  | "&&"                    { AND }
  | "||"                    { OR }

  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "["                     { LBRAC }
  | "]"                     { RBRAC }
  | "::"                    { COLONCOLON }
  | ";"                     { SEMI }

  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { MUL }
  | "/"                     { DIV }

  | ['0'-'9']+ as num       { Num(int_of_string num) }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str
                            { Id(str) }

  | _ { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
