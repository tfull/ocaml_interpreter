open Syntax

let _ =
    let rec loop tenv env =
        try
            let cmd = print_string "MyCaml> "; flush stdout; Parser.parse Tokenizer.tokenize (Lexing.from_channel stdin) in
            match cmd with
                | CLet (s, e) ->
                    let (vs, t) = Inference.infer tenv e in
                    let v = Evaluator.evaluate env e in
                    print_string ("val " ^ s ^ " : " ^ string_of_data_type t ^ " = " ^ string_of_value v ^ "\n");
                    flush stdout;
                    loop ((s, (vs, t)) :: tenv) ((s, v) :: env)
                | CExp e ->
                    let (vs, t) = Inference.infer tenv e in
                    let v = Evaluator.evaluate env e in
                    print_string ("- : " ^ string_of_data_type t ^ " = " ^ string_of_value v ^ "\n");
                    flush stdout;
                    loop tenv env
        with
            | TokenizeError s -> print_string ("TokenizeError: " ^ s ^ "\n"); flush stdout; loop tenv env
            | ParseError s -> print_string ("ParseError: " ^ s ^ "\n"); flush stdout; loop tenv env
            | EvaluateError s -> print_string ("EvaluateError: " ^ s ^ "\n"); flush stdout; loop tenv env
            | InferError s -> print_string ("InferError: " ^ s ^ "\n"); flush stdout; loop tenv env
            | ExitException -> print_string "Exit.\n"; flush stdout
    in
    loop [] []
