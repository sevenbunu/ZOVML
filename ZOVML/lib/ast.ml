(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | Int of (int[@gen QCheck.Gen.(0 -- Int.max_int)]) (** e.g. [18] *)
  | Bool of bool (** e.g. [True] *)
  | Unit (** () *)
[@@deriving qcheck, show { with_path = false }]

type 'a option =
  | None (** None *)
  | Some of 'a (** e.g. [Just 5] *)
[@@deriving qcheck, show { with_path = false }]

type is_rec =
  | Rec
  | Nonrec
[@@deriving qcheck, show { with_path = false }]

type tp =
  | TUnit (** () *)
  | TInt (** Int *)
  | TBool (** Bool *)
  | OptionParam of tp (** e.g. [Option Int]*)
  | ListParam of tp (** e.g. [[Int]] *)
  | TupleParams of tp * tp * tp_list (** e.g. [(Int, Bool)] *)
  | FunctionType of functype
[@@deriving qcheck, show { with_path = false }]

and functype = FuncT of tp * tp * tp_list (** e.g. [Int-> Bool -> (Int,Bool)] *)

and tp_list =
  (tp list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 10))) (gen_tp_sized (n / 10)))])
[@@deriving qcheck, show { with_path = false }]

type binop =
  | And (** [&&]*)
  | Or (** [||] *)
  | Plus (** [+] *)
  | Minus (** [-] *)
  | Divide (** [/] *)
  | Mod (** [%]*)
  | Cons (** [::] *)
  | Multiply (** [*] *)
  | Equality (** [=] *)
  | Pow (** [^] *)
  | Inequality (** [<>] *)
  | Less (** [<] *)
  | Greater (** [>] *)
  | EqualityOrLess (** [<=] *)
  | EqualityOrGreater (** [>=] *)
[@@deriving qcheck, show { with_path = false }]

let gen_first_symbol =
  QCheck.Gen.(
    map
      Char.chr
      (oneof [ int_range (Char.code 'a') (Char.code 'z'); return (Char.code '_') ]))
;;

let gen_char =
  QCheck.Gen.(
    map
      Char.chr
      (oneof
         [ int_range (Char.code 'a') (Char.code 'z')
         ; int_range (Char.code 'A') (Char.code 'Z')
         ; int_range (Char.code '0') (Char.code '9')
         ; return (Char.code '_')
         ; return (Char.code '\'')
         ]))
;;

let is_keyword_or_underscore = function
  | "case" | "of" | "if" | "then" | "else" | "let" | "in" | "where" | "_" -> true
  | _ -> false
;;

let varname =
  QCheck.Gen.(
    map2
      (fun x y -> Printf.sprintf "%c%s" x y)
      gen_first_symbol
      (string_size ~gen:gen_char (1 -- 7)))
;;

let correct_varname x = QCheck.Gen.map (fun y -> Printf.sprintf "%s%c" x y) gen_char

let gen_string =
  let open QCheck.Gen in
  let x = varname in
  map is_keyword_or_underscore x
  >>= fun y -> if y then map correct_varname x >>= fun y -> y else x
;;

type ident = Ident of (string[@gen gen_string])
[@@deriving qcheck, show { with_path = false }]

type pconst =
  | OrdinaryPConst of const (** e.g [True]*)
  | NegativePInt of (int[@gen QCheck.Gen.(0 -- Int.max_int)]) (** e.g [-12]*)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  | PWildcard (** _ *)
  | PConst of pconst
  | PIdentificator of ident (** e.g. [x] *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern_list (** e.g. [(x, y, z)]*)
  | POption of pattern option (** e.g. [Some x] *)

and listpat =
  | PCons of pattern * pattern (** e.g. [x:xs] *)
  | PEnum of pattern_list (** e.g. [[x; y; z]] *)

and pattern_list =
  (pattern list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) (gen_pattern_sized (n / 7)))])

type expr =
  | Identificator of ident
  | Const of const
  | Tuple of expr * expr * expr list
  | ESome
  | ENone
  | EList of expr list
  | Neg of expr
  | Let of let_definition * expr
  | IfThenEsle of expr * expr * expr option
  | FunctionApply of expr * expr * expr list
  | Binop of expr * binop * expr
  | Match of expr * pattern

and let_definition = Let_simple of is_rec * expr * expr list
