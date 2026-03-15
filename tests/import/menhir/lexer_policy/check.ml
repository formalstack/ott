let parse_lib_term s =
  let lexbuf = Lexing.from_string s in
  Lib_parser.term_start Lib_lexer.token lexbuf

let parse_main_wrapped s =
  let lexbuf = Lexing.from_string s in
  Main_parser.wrapped_start Main_lexer.token lexbuf

let expect_lib_comment () =
  match parse_lib_term "x // provider comment\n" with
  | Lib_ast.Term_var "x" -> ()
  | _ -> failwith "unexpected provider parse tree"

let expect_main_comment () =
  match parse_main_wrapped "x # local comment\n" with
  | Main_ast.Wrapped_wrap (Lib_ast.Term_var "x") -> ()
  | _ -> failwith "unexpected main parse tree"

let expect_main_rejects_imported_comment () =
  let lexbuf = Lexing.from_string "x // provider comment\n" in
  try
    ignore (Main_parser.wrapped_start Main_lexer.token lexbuf);
    failwith "expected imported comment marker to be rejected"
  with
  | Main_lexer.Error _ -> ()
  | Parsing.Parse_error -> ()

let () =
  expect_lib_comment ();
  expect_main_comment ();
  expect_main_rejects_imported_comment ();
  print_endline "ok"
