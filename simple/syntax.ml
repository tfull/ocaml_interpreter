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
    | ERec of string * string * expression
    | ENil
    | ECons of expression * expression
    | EPair of expression * expression
    | ETriple of expression * expression * expression
and value = 
    | VInt of int
    | VBool of bool
    | VFun of string * environment * expression
    | VRec of string * string * environment * expression
    | VList of value list
    | VPair of value * value
    | VTriple of value * value * value
and environment = (string * value) list

type command =
    | CLet of string * expression
    | CExp of expression

let rec string_of_value = function
    | VInt i -> string_of_int i
    | VBool b -> if b then "true" else "false"
    | VFun _ -> "function"
    | VRec _ -> "recursive function"
    | VList [] -> "[]"
    | VList (x :: xs) -> "[" ^ string_of_value x ^ string_of_value_list xs ^ "]"
    | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
    | VTriple (v1, v2, v3) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ", " ^ string_of_value v3 ^ ")"
and string_of_value_list = function
    | [] -> ""
    | x :: xs -> "; " ^ string_of_value x ^ string_of_value_list xs

exception ExitException
exception ParseError of string
exception TokenizeError of string
exception EvaluateError of string
