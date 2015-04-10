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
%token IF THEN ELSE
%token LPAR RPAR
%token LET REC IN
%token FUN ARROW
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
    | FUN funstate { $2 }
    | s { $1 }
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
    | a EQUAL a { EEq ($1, $3) }
    | a LT a { ELt ($1, $3) }
    | a GT a { EGt ($1, $3) }
    | a LE a { ELe ($1, $3) }
    | a GE a { EGe ($1, $3) }
    | a { $1 }
a:
    | a STAR b { EMul ($1, $3) }
    | a SLASH b { EDiv ($1, $3) }
    | a MOD b { EMod ($1, $3) }
    | b { $1 }
b:
    | b PLUS c { EAdd ($1, $3) }
    | b MINUS c { ESub ($1, $3) }
    | c { $1 }
c:
    | MINUS c { EMinus $2 }
    | d { $1 }
d:
    | d e { EApp ($1, $2) }
    | e { $1 }
e:
    | VAR { EVar $1 }
    | INT { EInt $1 }
    | BOOL { EBool $1 }
    | LPAR state RPAR { $2 }
;