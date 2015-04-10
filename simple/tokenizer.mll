{
    open Syntax
    open Parser
}

let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let digit = ['0' - '9']
let var = (lower | upper | digit | '_')

rule tokenize = parse
    | [' ' '\t' '\n'] { tokenize lexbuf }
    | ";;" { EOS }
    | ['0' - '9']+ as v { INT (int_of_string v) }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { STAR }
    | '/' { SLASH }
    | '<' { LT }
    | '>' { GT }
    | '=' { EQUAL }
    | '(' { LPAR }
    | ')' { RPAR }
    | "<=" { LE }
    | ">=" { GE }
    | "->" { ARROW }
    | "mod" { MOD }
    | "let" { LET }
    | "rec" { REC }
    | "in" { IN }
    | "fun" { FUN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "true" { BOOL true }
    | "false" { BOOL false }
    | lower (var)* as v { VAR v }
    | eof { raise ExitException }
    | _ { raise (TokenizeError "illegal character" ) }
