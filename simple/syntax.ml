type expression =
    | EInt of int
    | EBool of bool
    | EVar of string
    | EAdd of expression * expression
    | ESub of expression * expression
    | EMul of expression * expression
    | EDiv of expression * expression
    | EMod of expression * expression
    | EMinus of expression
    | ELt of expression * expression
    | ELe of expression * expression
    | EGt of expression * expression
    | EGe of expression * expression
    | EEq of expression * expression
    | EIf of expression * expression * expression
    | ELet of string * expression * expression

type command =
    | CLet of string * expression
    | CExp of expression

type value = 
    | VInt of int
    | VBool of bool
