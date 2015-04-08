open Syntax

exception EvaluateError of string

let string_of_value = function
    | VInt i -> string_of_int i
    | VBool b -> if b then "true" else "false"
    | VFun _ -> "fun"

let int_of_value = function
    | VInt i -> i
    | _ -> raise Exit

let bool_of_value = function
    | VBool b -> b
    | _ -> raise Exit

let rec evaluate env = function
    | EInt i -> VInt i
    | EBool b -> VBool b
    | EVar v -> List.assoc v env
    | EFun (v, e) -> VFun (v, env, e)
    | EApp (e1, e2) -> apply env e1 e2
    | ELet (v, e1, e2) -> evaluate ((v, evaluate env e1) :: env) e2
    | EAdd (e1, e2) ->
        VInt (int_of_value (evaluate env e1) + int_of_value (evaluate env e2))
    | ESub (e1, e2) ->
        VInt (int_of_value (evaluate env e1) - int_of_value (evaluate env e2))
    | EMul (e1, e2) ->
        VInt (int_of_value (evaluate env e1) * int_of_value (evaluate env e2))
    | EDiv (e1, e2) ->
        VInt (int_of_value (evaluate env e1) / int_of_value (evaluate env e2))
    | EMod (e1, e2) ->
        VInt (int_of_value (evaluate env e1) mod int_of_value (evaluate env e2))
    | EMinus e -> VInt (- int_of_value (evaluate env e))
    | ELt (e1, e2) ->
        VBool (int_of_value (evaluate env e1) < int_of_value (evaluate env e2))
    | ELe (e1, e2) ->
        VBool (int_of_value (evaluate env e1) <= int_of_value (evaluate env e2))
    | EGt (e1, e2) ->
        VBool (int_of_value (evaluate env e1) > int_of_value (evaluate env e2))
    | EGe (e1, e2) ->
        VBool (int_of_value (evaluate env e1) >= int_of_value (evaluate env e2))
    | EEq (e1, e2) ->
        VBool (int_of_value (evaluate env e1) = int_of_value (evaluate env e2))
    | EIf (e1, e2, e3) ->
        if bool_of_value (evaluate env e1) then
            evaluate env e2
        else
            evaluate env e3
and apply env e1 e2 =
    match evaluate env e1 with
        | VFun (vf, envf, ef) -> evaluate ((vf, evaluate env e2) :: envf) ef
        | _ -> raise (EvaluateError "apply (not function)")

let _ =
    let rec loop env =
        try
            let cmd = Parser.parse Tokenizer.tokenize (Lexing.from_channel stdin) in
            match cmd with
                | CLet (s, e) ->
                    let v = evaluate env e in
                    print_string ("let " ^ s ^ " = " ^ string_of_value v);
                    print_newline ();
                    flush stdout;
                    loop ((s, v) :: env)
                | CExp e ->
                    let v = evaluate env e in
                    print_string (string_of_value v);
                    print_newline ();
                    flush stdout;
                    loop env
        with _ -> print_string "Error\n"; flush stdout; loop env
    in
    loop []
