(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Core

let test_expr str = pp_result Ast.pp_expr @@ param_parse Parser.Expr.parse_expr str

let test_let_decl str =
  pp_result Ast.pp_let_deinition
  @@ param_parse (Parser.Expr.parse_let_declaration Parser.Expr.parse_expr) str
;;

let%expect_test "sum" =
  test_expr "5 + 5";
  [%expect
    {|
    (Expr_apply ((Expr_ident "( + )"), (Expr_const (Const_int 5)),
       [(Expr_const (Const_int 5))]))
    |}]
;;

let%expect_test "apply" =
  test_expr "f g";
  [%expect
    {|
    (Expr_apply ((Expr_ident "( + )"), (Expr_const (Const_int 5)),
       [(Expr_const (Const_int 5))]))
    |}]
;;

let%expect_test "arithmetics" =
  test_expr "5 + 5 - (5 * 7 + 5)";
  [%expect
    {|
    (Expr_apply ((Expr_ident "( - )"),
       (Expr_apply ((Expr_ident "( + )"), (Expr_const (Const_int 5)),
          [(Expr_const (Const_int 5))])),
       [(Expr_apply ((Expr_ident "( + )"),
           (Expr_apply ((Expr_ident "( * )"), (Expr_const (Const_int 5)),
              [(Expr_const (Const_int 7))])),
           [(Expr_const (Const_int 5))]))
         ]
       ))
    |}]
;;

let%expect_test "simple if" =
  test_expr "if n = 5 \n    then n * n else n - 1";
  [%expect
    {|
    (Expr_if (
       (Expr_apply ((Expr_ident "( = )"), (Expr_ident "n"),
          [(Expr_const (Const_int 5))])),
       (Expr_apply ((Expr_ident "( * )"), (Expr_ident "n"), [(Expr_ident "n")])),
       (Some (Expr_apply ((Expr_ident "( - )"), (Expr_ident "n"),
                [(Expr_const (Const_int 1))])))
       ))
    |}]
;;

let%expect_test "let decl test" =
  test_let_decl " let rec a =";
  [%expect
    {|
    (Expr_if (
       (Expr_apply ((Expr_ident "( = )"), (Expr_ident "n"),
          [(Expr_const (Const_int 5))])),
       (Expr_apply ((Expr_ident "( * )"), (Expr_ident "n"), [(Expr_ident "n")])),
       (Some (Expr_apply ((Expr_ident "( - )"), (Expr_ident "n"),
                [(Expr_const (Const_int 1))])))
       ))
    |}]
;;

let%expect_test "let test" =
  test_expr "let rec a = 5";
  [%expect
    {|
    (Expr_if (
       (Expr_apply ((Expr_ident "( = )"), (Expr_ident "n"),
          [(Expr_const (Const_int 5))])),
       (Expr_apply ((Expr_ident "( * )"), (Expr_ident "n"), [(Expr_ident "n")])),
       (Some (Expr_apply ((Expr_ident "( - )"), (Expr_ident "n"),
                [(Expr_const (Const_int 1))])))
       ))
    |}]
;;

let%expect_test "fac test" =
  test_expr "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)";
  [%expect
    {|
    (Expr_if (
       (Expr_apply ((Expr_ident "( = )"), (Expr_ident "n"),
          [(Expr_const (Const_int 5))])),
       (Expr_apply ((Expr_ident "( * )"), (Expr_ident "n"), [(Expr_ident "n")])),
       (Some (Expr_apply ((Expr_ident "( - )"), (Expr_ident "n"),
                [(Expr_const (Const_int 1))])))
       ))
    |}]
;;
