(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast

let fail = fail ""
let fail_if cond = if cond then fail else return ()
let skip_whitespace = skip_while Char.is_whitespace *> return ()

let parse_comment =
  skip_whitespace *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let choice_pass_prev l prev = choice (List.map ~f:(fun el -> el prev) l)
let ws = many parse_comment *> skip_whitespace

let wsl =
  let skip_ws1 = take_while1 Char.is_whitespace *> return () in
  (skip_ws1 *> many parse_comment <|> many1 parse_comment) *> return ()
;;

let ops_symbols = function
  | '*' | '&' | '$' | '+' | '@' | '/' | '=' | '>' | '-' | '^' -> true
  | _ -> false
;;

let oper_symbols a =
  ops_symbols a
  ||
  match a with
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' | '|' -> true
  | _ -> false
;;

let is_keyword = function
  | "if"
  | "then"
  | "else"
  | "let"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "fun"
  | "type"
  | "int"
  | "bool"
  | "when"
  | "function"
  | "and" -> true
  | _ -> false
;;

let parse_int = take_while1 Char.is_digit >>| fun str -> Const_int (Int.of_string str)

let parse_ident =
  let is_first_char = function
    | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first_char = satisfy is_first_char >>| Char.to_string in
  let* rest = take_while is_valid_char in
  let ident = first_char ^ rest in
  ws *> fail_if (is_keyword ident) *> return ident
;;
