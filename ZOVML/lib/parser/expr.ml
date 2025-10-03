(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast
open Common

let parse_const_int = ws *> parse_int >>| fun s -> Expr_const s

let parse_const_bool =
  ws *> choice [ string "true" *> return true; string "false" *> return false ]
  >>| fun s -> Expr_const (Const_bool s)
;;

let par_op s = "( " ^ s ^ " )"

let parse_const_string =
  ws
  *> char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  >>| fun s -> Expr_const (Const_string s)
;;

let parse_const = ws *> choice [ parse_const_int; parse_const_bool; parse_const_string ]

let prefix =
  let* first_el = char '!' in
  let+ other_el = take_while oper_symbols in
  Char.escaped first_el ^ other_el
;;

let prefix1 =
  let* first_el = char '?' <|> char '~' in
  let+ other_el = take_while1 oper_symbols in
  Char.escaped first_el ^ other_el
;;

let parse_arithmatics =
  let+ op = char '-' <|> char '+' in
  par_op ("~" ^ Char.escaped op)
;;

let parse_bop op =
  let+ op = ws *> op in
  fun ls rs -> Expr_apply (Expr_ident (par_op op), ls, [ rs ])
;;

let rec parse_uop func arg_parser =
  let* pfunc = ws *> func in
  let+ arg = parse_uop func arg_parser <|> arg_parser in
  Expr_apply (Expr_ident pfunc, arg, [])
;;

let parens_elimination x = ws *> char '(' *> ws *> x <* ws *> char ')'
let prefix_op parse prev = parse_uop parse prev <|> prev
let infix_left_op parser prev = chainl1 prev (parse_bop parser)
let infix_right_op parser prev = chainr1 prev (parse_bop parser)
let parse_ident_expr = ws *> parse_ident >>| fun s -> Expr_ident s
let parse_apply prev = chainl1 prev (return (fun e1 e2 -> Expr_apply (e1, e2, [])))

let parse_let_declaration _ =
  let+ _ = ws *> string "let"
  and+ rec_flag = ws *> string "rec" *> return Rec <|> return Nonrec
  and+ args = sep_by ws parse_ident_expr
  and+ _ = ws *> char '=' in
  Let_simple
    ( rec_flag
    , List.hd args
    , (fun s ->
         match s with
         | None -> []
         | Some l -> l)
        (List.tl args) )
;;

let parse_let prev =
  let+ let_decl = parse_let_declaration prev
  and+ e =
    option
      (Expr_const (Const_bool true))
      (let+ e = prev in
       e)
  in
  Expr_let (let_decl, e)
;;

let parse_if prev =
  let+ _ = ws *> string "if"
  and+ expr_if = prev
  and+ _ = ws *> string "then"
  and+ expr_then = prev
  and+ expr_else =
    option
      None
      (let+ _ = ws *> string "else"
       and+ e = prev in
       Some e)
  in
  Expr_if (expr_if, expr_then, expr_else)
;;

let parse_prefix_op = prefix <|> prefix1

let parse_infix_op prefix =
  let case1 =
    let+ first_el = char '|' <|> char '#'
    and+ other_el = take_while1 oper_symbols in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let+ first_el = satisfy ops_symbols <|> char '<' <|> char '%'
    and+ other_el = take_while oper_symbols in
    Char.escaped first_el ^ other_el
  in
  let* check_prefix = peek_string @@ String.length prefix in
  if String.equal check_prefix prefix then case1 <|> case2 else fail
;;

let parse_infix_with_prefixes prefixes = choice (List.map ~f:parse_infix_op prefixes)

let parse_expr =
  let parse_selected l init =
    let rec traversal prev = function
      | h :: tl -> traversal (h prev) tl
      | [] -> prev
    in
    traversal init l
  in
  fix (fun self ->
    parse_selected
      [ prefix_op @@ parse_prefix_op
      ; infix_left_op @@ parse_infix_op "#"
      ; choice_pass_prev [ parse_apply ]
      ; infix_right_op @@ parse_infix_op "**"
      ; prefix_op @@ parse_arithmatics
      ; infix_left_op @@ parse_infix_with_prefixes [ "*"; "/"; "%" ]
      ; infix_left_op @@ parse_infix_with_prefixes [ "+"; "-" ]
      ; infix_right_op @@ parse_infix_with_prefixes [ "@"; "^" ]
      ; infix_left_op
        @@ choice
             [ parse_infix_with_prefixes [ "="; "<"; ">"; "|"; "&"; "$" ]; string "!=" ]
      ; infix_right_op @@ choice [ string "&"; string "&&" ]
      ; infix_right_op @@ choice [ string "or"; string "||" ]
      ; infix_right_op @@ choice [ string "<-"; string ":=" ]
      ; choice_pass_prev [ parse_if; Fn.id ]
      ; choice_pass_prev @@ [ parse_let ] @ [ Fn.id ]
      ]
      (choice
         [ parse_const
         ; parse_ident_expr
         ; parens_elimination self
         ; choice_pass_prev [ parse_let ] self
         ; parse_if self
         ]))
;;
