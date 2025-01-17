open Liteeffects.Ast_utils
open Liteeffects.Typecheck_utils

let ast_testable = Alcotest.testable pp_ast ( = )
let typecheck_error_testable = Alcotest.testable pp_type_error ( = )
let typ_testable = Alcotest.testable pp_ttype ( = )
let effect_set_testable = Alcotest.testable pp_effects ( = )
