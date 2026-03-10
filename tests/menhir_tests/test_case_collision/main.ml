open Test_case_collision_ast

module Lexer = Test_case_collision_lexer
module Parser = Test_case_collision_parser
module PP = Test_case_collision_parser_pp

let _parse_ty : Lexing.lexbuf -> ty = Parser.ty_start Lexer.token

let _pp_raw_ty : ty -> PPrint.document = PP.pp_raw_ty

let _pp_ty : ty -> PPrint.document = PP.pp_ty

let () = ()
