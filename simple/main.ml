open Syntax

let _ =
    let rec loop env =
        try
            let cmd = print_string "MyCaml> "; flush stdout; Parser.parse Tokenizer.tokenize (Lexing.from_channel stdin) in
            match cmd with
                | CLet (s, e) ->
                    let v = Evaluator.evaluate env e in
                    print_string ("let " ^ s ^ " = " ^ string_of_value v ^ "\n");
                    flush stdout;
                    loop ((s, v) :: env)
                | CExp e ->
                    let v = Evaluator.evaluate env e in
                    print_string (string_of_value v ^ "\n");
                    flush stdout;
                    loop env
        with
            | TokenizeError s -> print_string ("TokenizeError: " ^ s ^ "\n"); flush stdout; loop env
            | ParseError s -> print_string ("ParseError: " ^ s ^ "\n"); flush stdout; loop env
            | EvaluateError s -> print_string ("EvaluateError: " ^ s ^ "\n"); flush stdout; loop env
            | ExitException -> print_string "Exit.\n"; flush stdout
    in
    loop []
