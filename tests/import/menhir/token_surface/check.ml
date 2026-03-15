let parse s =
  let lexbuf = Lexing.from_string s in
  Main_parser.wrapped_start Main_lexer.token lexbuf

let rejected_bang () =
  let lexbuf = Lexing.from_string "!" in
  try
    ignore (Main_parser.wrapped_start Main_lexer.token lexbuf);
    false
  with
  | Main_lexer.Error _
  | Main_parser.Error -> true

let () =
  begin
    match parse "x" with
    | Main_ast.Wrapped_wrap (Lib_ast.Term_var "x") -> ()
    | _ -> failwith "expected x to parse through the root parser"
  end;
  if not (rejected_bang ()) then
    failwith "expected ! to be rejected by the root lexer/parser";
  print_endline "ok"
