(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf_ast

let list_to_string pp sep lst =
  let rec aux = function
    | [] -> ""
    | [ x ] -> pp x
    | x :: xs -> Printf.sprintf "%s%s%s" (pp x) sep (aux xs)
  in
  aux lst
;;

let rec print_imm = function
  | ImmInt i -> string_of_int i
  | ImmBool false -> "false"
  | ImmBool true -> "true"
  | ImmIdentifier id -> id
  | ImmUnit -> "()"
  | ImmTuple tup -> Printf.sprintf "(%s)" (list_to_string print_imm ", " tup)
;;

let rec print_cexpr = function
  | CImmExpr imm -> print_imm imm
  | CIfThenElse (cond, then_branch, else_branch) ->
    Printf.sprintf
      "if %s then %s else %s"
      (print_imm cond)
      (print_aexpr then_branch)
      (print_aexpr else_branch)
  | CApplication (left, right, args) ->
    Printf.sprintf
      "%s %s %s"
      (print_imm left)
      (print_imm right)
      (args |> List.map print_imm |> String.concat " ")

and print_aexpr = function
  | AExpr cexp -> print_cexpr cexp
  | ALetIn (pat, outer, inner) ->
    Printf.sprintf "let %s = %s in\n%s" pat (print_cexpr outer) (print_aexpr inner)

and print_binding = function
  | Let (bnm, agrs, cexp) ->
    Printf.sprintf "let %s %s = %s;;" bnm (agrs |> String.concat " ") (print_aexpr cexp)

and print_bindings bl = List.map (fun f -> print_binding f) bl |> String.concat " \n"
