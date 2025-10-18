(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | Int of (int[@gen QCheck.Gen.(0 -- Int.max_int)])
  | Bool of bool
  | Unit
[@@deriving show { with_path = false }]

type 'a toption =
  | TNone
  | TSome of 'a
[@@deriving show { with_path = false }]

type is_rec =
  | Rec
  | Nonrec
[@@deriving show { with_path = false }]

type tp =
  | TUnit
  | TInt
  | TBool
  | OptionParam of tp
  | ListParam of tp
  | TupleParams of tp * tp * tp_list
  | FunctionType of functype
[@@deriving show { with_path = false }]

and functype = FuncT of tp * tp * tp_list

and tp_list =
  (tp list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 10))) (gen_tp_sized (n / 10)))])
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

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

type ident = Ident of (string[@gen gen_string]) [@@deriving show { with_path = false }]

type pconst =
  | OrdinaryPConst of const
  | NegativePInt of (int[@gen QCheck.Gen.(0 -- Int.max_int)])
[@@deriving show { with_path = false }]

type pattern =
  (ident list[@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) gen_ident)])
  * pat
  * tp_list
[@@deriving show { with_path = false }]

and pattern_list =
  (pattern list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) (gen_pattern_sized (n / 7)))])

and pat =
  | PWildcard (** _ *)
  | PConst of pconst
  | PIdentificator of ident (** e.g. [x] *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern_list (** e.g. [(x, y, z)]*)
  | POption of pattern toption
[@@deriving show { with_path = false }]

and listpat =
  | PCons of pattern * pattern (** e.g. [x:xs] *)
  | PEnum of pattern_list (** e.g. [[x; y; z]] *)

type expression =
  | Identificator of ident
  | Const of const
  | Tuple of expr * expr * expr list
  | ESome
  | ENone
  | EList of expr list
  | Neg of expr
  | Let of let_definition * expr
  | IfThenEsle of expr * expr * expr toption
  | FunctionApply of expr * expr * expr list
  | Binop of expr * binop * expr
  | Match of expr * (pattern * expr) list
  | AnonFunc of pattern * pattern_list * expr

and let_definition = Let_simple of is_rec * pattern * expr
and expr = expression * tp_list [@@deriving show { with_path = false }]

type def =
  | VarsDef of pattern * expr
  | FunDef of is_rec * ident * pattern * pattern_list * expr
[@@deriving show { with_path = false }]

and binding =
  | Def of def
  | Decl of ident * tp
