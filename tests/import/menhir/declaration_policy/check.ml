let parse_term s =
  let lexbuf = Lexing.from_string s in
  Rename_parser.term_start Rename_lexer.token lexbuf

let () =
  match parse_term "x y z" with
  | Rename_ast.Term_app
      (Rename_ast.Term_app (Rename_ast.Term_var "x", Rename_ast.Term_var "y"),
       Rename_ast.Term_var "z") ->
      print_endline "ok"
  | _ -> failwith "unexpected parse tree"
