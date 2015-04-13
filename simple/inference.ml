open Syntax

let nub l =
    let rec sub ys = function
        | [] -> ys
        | x :: xs -> if List.mem x ys then sub ys xs else sub (x :: ys) xs
    in
    sub [] l

let rec substitute t a x =
    match t with
        | TInt -> TInt
        | TBool -> TBool
        | TFun (t1, t2) -> TFun (substitute t1 a x, substitute t2 a x)
        | TList t -> TList (substitute t a x)
        | TPair (t1, t2) -> TPair (substitute t1 a x, substitute t2 a x)
        | TTriple (t1, t2, t3) -> TTriple (substitute t1 a x, substitute t2 a x, substitute t3 a x)
        | TVar v as tv -> if tv = a then x else tv

let instanciate i (vs, t) =
    let rec iterate i = function
        | [] -> []
        | x :: xs -> (TVar x, TVar i) :: iterate (i + 1) xs
    in
    (List.fold_left (fun t (a, b) -> substitute t a b) t (iterate i vs), i + List.length vs)

let rec lookup i x = function
    | [] -> raise (EvaluateError ("no such variable " ^ x))
    | (k, v) :: xs -> if x = k then instanciate i v else lookup i x xs

let rec occur alpha = function
    | TInt -> false
    | TBool -> false
    | TFun (a, b) -> occur alpha a || occur alpha b
    | TVar a as tv -> tv = alpha
    | TList a -> occur alpha a
    | TPair (a, b) -> occur alpha a || occur alpha b
    | TTriple (a, b, c) -> occur alpha a || occur alpha b || occur alpha c

let rec gather_type_variables = function
    | TInt -> []
    | TBool -> []
    | TFun (a, b) -> nub (gather_type_variables a @ gather_type_variables b)
    | TVar a -> [a]
    | TList a -> gather_type_variables a
    | TPair (a, b) -> nub (gather_type_variables a @ gather_type_variables b)
    | TTriple (a, b, c) -> nub (gather_type_variables a @ gather_type_variables b @ gather_type_variables c)

let rec gather_unit_variables (vs, t) =
    match t with
        | TInt -> []
        | TBool -> []
        | TFun (a, b) -> nub (gather_unit_variables (vs, a) @ gather_unit_variables (vs, b))
        | TVar a -> if not (List.mem a vs) then [a] else []
        | TList a -> gather_unit_variables (vs, a)
        | TPair (a, b) -> nub (gather_unit_variables (vs, a) @ gather_unit_variables (vs, b))
        | TTriple (a, b, c) -> nub (gather_unit_variables (vs, a) @ gather_unit_variables (vs, b) @ gather_unit_variables (vs, c))

let generalize t = (gather_type_variables t, t)

let substitute_select (vs, t) (a, b) =
    match a with
        | TVar x -> if List.mem x vs then (vs, t) else (vs, substitute t a b)
        | _ -> raise (InferError "should type variable")

let rec substitute_environment sigma = function
    | [] -> []
    | (k, (vs, t)) :: xs ->
        let n = List.fold_left (fun ts (a, b) -> substitute_select ts (a, b)) (vs, t) sigma in
        (k, n) :: substitute_environment sigma xs

let (--) xs ys =
    let rec f xs ys =
        match xs with
            | [] -> []
            | x :: xs -> if List.mem x ys then f xs ys else x :: f xs ys
    in
    f xs ys

let rec gather_pattern_constraints i = function
    | PInt _ -> (TInt, [], i, [])
    | PBool _ -> (TBool, [], i, [])
    | PVar a ->
        let alpha = TVar i in
        (alpha, [], i + 1, [(a, ([], alpha))])
    | PNil ->
        let alpha = TList (TVar i) in
        (alpha, [], i + 1, [])
    | PCons (p1, p2) ->
        let (t1, c1, i1, env1) = gather_pattern_constraints i p1 in
        let (t2, c2, i2, env2) = gather_pattern_constraints i1 p2 in
        (t2, (TList t1, t2) :: c1 @ c2, i2, env1 @ env2)
    | PPair (p1, p2) ->
        let (t1, c1, i1, env1) = gather_pattern_constraints i p1 in
        let (t2, c2, i2, env2) = gather_pattern_constraints i1 p2 in
        (TPair (t1, t2), c1 @ c2, i2, env1 @ env2)
    | PTriple (p1, p2, p3) ->
        let (t1, c1, i1, env1) = gather_pattern_constraints i p1 in
        let (t2, c2, i2, env2) = gather_pattern_constraints i1 p2 in
        let (t3, c3, i3, env3) = gather_pattern_constraints i2 p3 in
        (TTriple (t1, t2, t3), c1 @ c2 @ c3, i3, env1 @ env2 @ env3)

let rec gather_constraints env i = function
    | EInt _ -> (TInt, [], i)
    | EBool _ -> (TBool, [], i)
    | EVar v -> let (t, j) = lookup i v env in (t, [], j)
    | EAdd (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | ESub (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EMul (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EDiv (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EMod (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EMinus e ->
        let (t, c, j) = gather_constraints env i e in
        (TInt, (t, TInt) :: c, j)
    | ELt (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TBool, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | ELe (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TBool, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EGt (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TBool, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EGe (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TBool, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EEq (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TBool, (t1, TInt) :: (t2, TInt) :: c1 @ c2, i2)
    | EIf (e1, e2, e3) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        let (t3, c3, i3) = gather_constraints env i2 e3 in
        (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3, i3)
    | EFun (v, e) ->
        let tv = TVar i in
        let (t, c, j) = gather_constraints ((v, ([], tv)) :: env) (i + 1) e in
        (TFun (tv, t), c, j)
    | EApp (e1, e2) ->
        let tv = TVar i in
        let (t1, c1, i1) = gather_constraints env (i + 1) e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (tv, (t1, TFun (t2, tv)) :: c1 @ c2, i2)
    | ERec (name, v, e) ->
        let alpha = TVar i in
        let beta = TVar (i + 1) in
        let gamma = (name, ([], TFun (alpha, beta))) :: env in
        let (t, c, j) = gather_constraints ((v, ([], alpha)) :: gamma) (i + 2) e in
        (TFun (alpha, beta), (beta, t) :: c, j)
    | ELet (v, e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let sigma = unify c1 in
        let t1d = List.fold_left (fun t (a, b) -> substitute t a b) t1 sigma in
        let envd = substitute_environment sigma env in
        let p = gather_type_variables t1 -- (List.concat (List.map gather_unit_variables (List.map snd env))) in
        let (t2, c2, i2) = gather_constraints ((v, (p, t1d)) :: envd) i1 e2 in
        (t2, c1 @ c2, i2)
    | ENil -> (TList (TVar i), [], i + 1)
    | ECons (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (t2, (TList t1, t2) :: c1 @ c2, i2)
    | EPair (e1, e2) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        (TPair (t1, t2), c1 @ c2, i2)
    | ETriple (e1, e2, e3) ->
        let (t1, c1, i1) = gather_constraints env i e1 in
        let (t2, c2, i2) = gather_constraints env i1 e2 in
        let (t3, c3, i3) = gather_constraints env i2 e3 in
        (TTriple (t1, t2, t3), c1 @ c2 @ c3, i3)
    | EMatch (e, xs) ->
        let (t0, c0, i0) = gather_constraints env i e in
        let beta = TVar i0 in
        let (t1, c1, i1) = pattern_match env (i0 + 1) t0 beta xs in
        (t1, c0 @ c1, i1)
and pattern_match env i alpha beta = function
    | [] -> (beta, [], i)
    | (p, e) :: xs ->
        let (t1, c1, i1, ad) = gather_pattern_constraints i p in
        let (t2, c2, i2) = gather_constraints (ad @ env) i1 e in
        let (_, c3, i3) = pattern_match env i2 alpha beta xs in
        (beta, (alpha, t1) :: (beta, t2) :: c1 @ c2 @ c3, i3)
and unify = function
    | [] -> []
    | (TInt, TInt) :: xs -> unify xs
    | (TBool, TBool) :: xs -> unify xs
    | (TFun (a1, b1), TFun (a2, b2)) :: xs ->
        unify ((a1, a2) :: (b1, b2) :: xs)
    | (TVar a, t) :: xs | (t, TVar a) :: xs ->
        let alpha = TVar a in
        if not (alpha = t) && occur alpha t then
            raise (InferError "recursive type reference")
        else
            (alpha, t) :: unify (List.map (fun (a, b) -> (substitute a alpha t, substitute b alpha t)) xs)
    | (TList a, TList b) :: xs -> unify ((a, b) :: xs)
    | (TPair (a1, a2), TPair (b1, b2)) :: xs -> unify ((a1, b1) :: (a2, b2) :: xs)
    | (TTriple (a1, a2, a3), TTriple (b1, b2, b3)) :: xs -> unify ((a1, b1) :: (a2, b2) :: (a3, b3) :: xs)
    | _ -> raise (InferError "not matched")
and infer env exp =
    let (t, cs, _) = gather_constraints env 1 exp in
    let ss = unify cs in
    let tk = List.fold_left (fun t (a, b) -> substitute t a b) t ss in
    generalize tk
