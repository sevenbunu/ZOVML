(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_bool of bool
[@@deriving show { with_path = false }]

type is_rec =
  | Rec
  | Nonrec
[@@deriving show { with_path = false }]

type let_deinition = Let_simple of is_rec * expr option * expr list
[@@deriving show { with_path = false }]

and expr =
  | Expr_ident of string
  | Expr_const of const
  | Expr_let of let_deinition * expr
  | Expr_apply of expr * expr * expr list
  | Expr_if of expr * expr * expr option
[@@deriving show { with_path = false }]

type statement =
  | Stmt_expr of expr
  | Stmt_let of let_deinition
[@@deriving show { with_path = false }]

type code = statement list [@@deriving show { with_path = false }]
