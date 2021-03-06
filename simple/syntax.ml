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
    | EMatch of expression * (pattern * expression) list
and value = 
    | VInt of int
    | VBool of bool
    | VFun of string * environment * expression
    | VRec of string * string * environment * expression
    | VNil
    | VCons of value * value
    | VPair of value * value
    | VTriple of value * value * value
and pattern =
    | PInt of int
    | PBool of bool
    | PPair of pattern * pattern
    | PTriple of pattern * pattern * pattern
    | PNil
    | PCons of pattern * pattern
    | PVar of string
and environment = (string * value) list

type command =
    | CLet of string * expression
    | CExp of expression

type data_type =
    | TInt
    | TBool
    | TFun of data_type * data_type
    | TList of data_type
    | TPair of data_type * data_type
    | TTriple of data_type * data_type * data_type
    | TVar of int

type type_schema = (int list) * data_type

type type_environment = (string * type_schema) list

exception ExitException
exception ParseError of string
exception TokenizeError of string
exception EvaluateError of string
exception InferError of string

let rec string_of_value = function
    | VInt i -> string_of_int i
    | VBool b -> if b then "true" else "false"
    | VFun _ -> "function"
    | VRec _ -> "recursive function"
    | VNil -> "[]"
    | VCons (x, y) -> "[" ^ string_of_value x ^ string_of_value_list y ^ "]"
    | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
    | VTriple (v1, v2, v3) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ", " ^ string_of_value v3 ^ ")"
and string_of_value_list = function
    | VNil -> ""
    | VCons (x, xs) -> "; " ^ string_of_value x ^ string_of_value_list xs
    | _ -> raise (Failure "string_of_value_list")

let rec string_of_data_type = function
    | TInt -> "int"
    | TBool -> "bool"
    | TFun (a, b) -> "(" ^ string_of_data_type a ^ " -> " ^ string_of_data_type b ^ ")"
    | TVar a -> "type" ^ string_of_int a
    | TList a -> "(" ^ string_of_data_type a ^ " list" ^ ")"
    | TPair (a, b) -> "(" ^ string_of_data_type a ^ " * " ^ string_of_data_type b ^ ")"
    | TTriple (a, b, c) -> "(" ^ string_of_data_type a ^ " * " ^ string_of_data_type b ^ " * " ^ string_of_data_type c ^ ")"
