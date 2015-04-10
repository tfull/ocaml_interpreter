open Syntax

let int_of_value = function
    | VInt i -> i
    | _ -> raise (EvaluateError "int conversion of other type")

let bool_of_value = function
    | VBool b -> b
    | _ -> raise (EvaluateError "bool conversion of other type")

let rec search s = function
    | [] -> raise (EvaluateError ("no such variable " ^ s))
    | (k, v) :: xs -> if s = k then v else search s xs

let rec evaluate env = function
    | EInt i -> VInt i
    | EBool b -> VBool b
    | EVar v -> search v env
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
        let v2 = int_of_value (evaluate env e2) in
        if v2 = 0 then
            raise (EvaluateError "$ / 0")
        else
            VInt (int_of_value (evaluate env e1) / int_of_value (evaluate env e2))
    | EMod (e1, e2) ->
        let v2 = int_of_value (evaluate env e2) in
        if v2 = 0 then
            raise (EvaluateError "$ % 0")
        else
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
    | ERec (f, v, e) -> VRec (f, v, env, e)
and apply env e1 e2 =
    match evaluate env e1 with
        | VFun (vf, envf, ef) -> evaluate ((vf, evaluate env e2) :: envf) ef
        | VRec (f, v, c, e) as funp -> evaluate ((v, evaluate env e2) :: (f, funp) :: c) e
        | _ -> raise (EvaluateError "apply (not function)")
