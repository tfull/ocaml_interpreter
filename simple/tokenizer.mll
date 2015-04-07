{
    open Parser
    exception EndOfInput
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
    | '%' { PERCENT }
    | '<' { LT }
    | '>' { GT }
    | '=' { EQUAL }
    | '(' { LPAR }
    | ')' { RPAR }
    | "<=" { LE }
    | ">=" { GE }
    | "let" { LET }
    | "in" { IN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "true" { BOOL true }
    | "false" { BOOL false }
    | lower (var)* as v { VAR v }
    | eof { raise EndOfInput }
