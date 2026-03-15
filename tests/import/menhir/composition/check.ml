let parse_term s =
  let lexbuf = Lexing.from_string s in
  Main_parser.consumer_start Main_lexer.token lexbuf

let parsed = parse_term "x"

let () =
  match parsed with
  | Lib_ast.Term_var "x" -> ()
  | _ -> failwith "unexpected parse tree"

module Main_pp = Main_parser_pp.Make(struct
  module Lib_ast_pp = Lib_parser_pp
end)

let _ : Main_ast.wrapped -> PPrint.document = Main_pp.pp_wrapped

let _ : Main_ast.wrapped -> PPrint.document = Main_pp.pp_raw_wrapped

let wrapped = Main_ast.Wrapped_wrap (Lib_ast.Term_var "x")

let () =
  PPrint.ToChannel.compact stdout (Lib_parser_pp.pp_raw_term parsed);
  output_char stdout '\n';
  PPrint.ToChannel.compact stdout (Main_pp.pp_raw_wrapped wrapped);
  output_char stdout '\n';
  PPrint.ToChannel.compact stdout (Main_pp.pp_wrapped wrapped);
  output_char stdout '\n'
