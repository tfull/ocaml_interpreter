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

let rec match_satisfy v = function
    | PInt i -> if i = int_of_value v then (true, []) else (false, [])
    | PBool b -> if b = bool_of_value v then (true, []) else (false, [])
    | PVar x -> (true, [(x, v)])
    | PNil -> if v = VNil then (true, []) else (false, [])
    | PCons (p1, p2) ->
        begin
            match v with
                | VCons (v1, v2) ->
                    let (b1, x1) = match_satisfy v1 p1 in
                    let (b2, x2) = match_satisfy v2 p2 in
                    if b1 && b2 then (true, x1 @ x2) else (false, [])
                | _ -> (false, [])
        end
    | PPair (p1, p2) ->
        begin
            match v with
                | VPair (v1, v2) ->
                    let (b1, x1) = match_satisfy v1 p1 in
                    let (b2, x2) = match_satisfy v2 p2 in
                    if b1 && b2 then (true, x1 @ x2) else (false, [])
                | _ -> (false, [])
        end
    | PTriple (p1, p2, p3) ->
        begin
            match v with
                | VTriple (v1, v2, v3) ->
                    let (b1, x1) = match_satisfy v1 p1 in
                    let (b2, x2) = match_satisfy v2 p2 in
                    let (b3, x3) = match_satisfy v3 p3 in
                    if b1 && b2 && b3 then (true, x1 @ x2 @ x3) else (false, [])
                | _ -> (false, [])
        end

let rec evaluate env = function
    | EInt i -> VInt i
    | EBool b -> VBool b
    | EVar v -> search v env
    | ENil -> VNil
    | EPair (e1, e2) -> VPair (evaluate env e1, evaluate env e2)
    | ETriple (e1, e2, e3) -> VTriple (evaluate env e1, evaluate env e2, evaluate env e3)
    | ECons (e1, e2) -> VCons (evaluate env e1, evaluate env e2)
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
    | ENe (e1, e2) ->
        VBool (int_of_value (evaluate env e1) != int_of_value (evaluate env e2))
    | EIf (e1, e2, e3) ->
        if bool_of_value (evaluate env e1) then
            evaluate env e2
        else
            evaluate env e3
    | ERec (f, v, e) -> VRec (f, v, env, e)
    | EMatch (e, ps) -> pattern_match env e ps
and apply env e1 e2 =
    match evaluate env e1 with
        | VFun (vf, envf, ef) -> evaluate ((vf, evaluate env e2) :: envf) ef
        | VRec (f, v, c, e) as funp -> evaluate ((v, evaluate env e2) :: (f, funp) :: c) e
        | _ -> raise (EvaluateError "apply (not function)")
and pattern_match env e = function
    | [] -> raise (EvaluateError "pattern not matched")
    | (p0, e0) :: ps ->
        let v = evaluate env e in
        let (b, xs) = match_satisfy v p0 in
        if b then
            evaluate (xs @ env) e0
        else
            pattern_match env e ps
