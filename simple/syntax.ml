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
    | EFun of string * expression
    | EApp of expression * expression
and value = 
    | VInt of int
    | VBool of bool
    | VFun of string * environment * expression
and environment = (string * value) list

type command =
    | CLet of string * expression
    | CExp of expression

let string_of_value = function
    | VInt i -> string_of_int i
    | VBool b -> if b then "true" else "false"
    | VFun _ -> "fun"

exception ExitException
exception ParseError of string
exception TokenizeError of string
exception EvaluateError of string
