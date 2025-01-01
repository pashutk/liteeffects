open Liteeffects.Ast_utils
open Liteeffects.Typecheck_utils

let ast_testable = Alcotest.testable pp_ast ( = )
let typecheck_error_testable = Alcotest.testable pp_type_error ( = )
