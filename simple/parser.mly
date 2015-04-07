%{
    open Syntax
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MINUS STAR SLASH PERCENT
%token LT GT EQUAL
%token LE GE
%token IF THEN ELSE
%token LPAR RPAR
%token LET IN
%token EOS

%start parse
%type <Syntax.command> parse
%%

parse:
    | command EOS { $1 }
command:
    | LET VAR EQUAL state { CLet ($2, $4) }
    | state { CExp $1 }
state:
    | LET VAR EQUAL state IN state { ELet ($2, $4, $6) }
    | IF state THEN state ELSE state { EIf ($2, $4, $6) }
    | s { $1 }
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
    | a PERCENT b { EMod ($1, $3) }
    | b { $1 }
b:
    | b PLUS c { EAdd ($1, $3) }
    | b MINUS c { ESub ($1, $3) }
    | c { $1 }
c:
    | MINUS c { EMinus $2 }
    | d { $1 }
d:
    | VAR { EVar $1 }
    | INT { EInt $1 }
    | BOOL { EBool $1 }
    | LPAR s RPAR { $2 }
;