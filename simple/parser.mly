%{
    open Syntax
    let parse_error s = raise (ParseError s)
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MINUS STAR SLASH MOD
%token LT GT EQUAL
%token LE GE
%token AND OR
%token IF THEN ELSE
%token LPAR RPAR
%token LET REC IN
%token FUN ARROW
%token SCOLON LSQ RSQ CONS
%token COMMA
%token MATCH WITH VERTICAL
%token EOS

%start parse
%type <Syntax.command> parse
%%

parse:
    | command EOS { $1 }
command:
    | LET VAR EQUAL state { CLet ($2, $4) }
    | LET VAR funlet { CLet ($2, $3) }
    | LET REC VAR VAR reclet { CLet ($3, ERec($3, $4, $5)) }
    | state { CExp $1 }
state:
    | LET VAR EQUAL state IN state { ELet ($2, $4, $6) }
    | LET REC VAR VAR reclet IN state { ELet ($3, ERec($3, $4, $5), $7) }
    | LET VAR funlet IN state { ELet ($2, $3, $5) }
    | IF state THEN state ELSE state { EIf ($2, $4, $6) }
    | MATCH state WITH match_s { EMatch ($2, $4) }
    | FUN funstate { $2 }
    | s { $1 }
match_s:
    | VERTICAL pattern_s ARROW state_m match_s { ($2, $4) :: $5 }
    | VERTICAL pattern_s ARROW state { [($2, $4)] }
state_m:
    | LET VAR EQUAL state IN state_m { ELet ($2, $4, $6) }
    | LET REC VAR VAR reclet IN state_m { ELet ($3, ERec($3, $4, $5), $7) }
    | LET VAR funlet IN state_m { ELet ($2, $3, $5) }
    | IF state THEN state ELSE state_m { EIf ($2, $4, $6) }
    | FUN funstate_m { $2 }
    | s { $1 }
funstate_m:
    | VAR funstate_m { EFun ($1, $2) }
    | VAR ARROW state_m { EFun ($1, $3) }
pattern_s:
    | pattern_a CONS pattern_s { PCons ($1, $3) }
    | pattern_a { $1 }
pattern_a:
    | INT { PInt $1 }
    | BOOL { PBool $1 }
    | VAR { PVar $1 }
    | LSQ RSQ { PNil }
    | LSQ pattern_ls RSQ { $2 }
    | LPAR pattern_s COMMA pattern_s RPAR { PPair ($2, $4) }
    | LPAR pattern_s COMMA pattern_s COMMA pattern_s RPAR { PTriple ($2, $4, $6) }
    | LPAR pattern_s RPAR { $2 }
pattern_ls:
    | pattern_s SCOLON pattern_ls { PCons ($1, $3) }
    | pattern_s { PCons ($1, PNil) }
funlet:
    | VAR funlet { EFun ($1, $2) }
    | VAR EQUAL state { EFun ($1, $3) }
funstate:
    | VAR funstate { EFun ($1, $2) }
    | VAR ARROW state { EFun ($1, $3) }
reclet:
    | VAR reclet { EFun ($1, $2) }
    | EQUAL state { $2 }
s:
    | s OR lg1 { EIf ($1, EBool true, $3) }
    | lg1 { $1 }
lg1:
    | lg1 AND lg2 { EIf ($1, $3, EBool false) }
    | lg2 { $1 }
lg2:
    | ss EQUAL ss { EEq ($1, $3) }
    | ss LT ss { ELt ($1, $3) }
    | ss GT ss { EGt ($1, $3) }
    | ss LE ss { ELe ($1, $3) }
    | ss GE ss { EGe ($1, $3) }
    | ss { $1 }
ss:
    | a CONS ss { ECons ($1, $3) }
    | a { $1 }
a:
    | a PLUS b { EAdd ($1, $3) }
    | a MINUS b { ESub ($1, $3) }
    | b { $1 }
b:
    | b STAR c { EMul ($1, $3) }
    | b SLASH c { EDiv ($1, $3) }
    | b MOD c { EMod ($1, $3) }
    | c { $1 }
c:
    | MINUS c { EMinus $2 }
    | d { $1 }
d:
    | d e { EApp ($1, $2) }
    | e { $1 }
e:
    | LSQ RSQ { ENil }
    | LSQ ls RSQ { $2 }
    | VAR { EVar $1 }
    | INT { EInt $1 }
    | BOOL { EBool $1 }
    | LPAR state RPAR { $2 }
    | LPAR state COMMA state RPAR { EPair ($2, $4) }
    | LPAR state COMMA state COMMA state RPAR { ETriple ($2, $4, $6) }
ls:
    | state SCOLON ls { ECons ($1, $3) }
    | state { ECons ($1, ENil) }
;