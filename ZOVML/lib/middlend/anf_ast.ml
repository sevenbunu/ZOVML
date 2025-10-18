(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmInt of int
  | ImmIdentifier of string
  | ImmBool of bool
  | ImmUnit
  | ImmTuple of immexpr list
[@@deriving show { with_path = false }]

type cexpr =
  | CApplication of immexpr * immexpr * immexpr list
  | CIfThenElse of immexpr * aexpr * aexpr
  | CImmExpr of immexpr
[@@deriving show { with_path = false }]

and aexpr =
  | ALetIn of string * cexpr * aexpr
  | AExpr of cexpr
[@@deriving show { with_path = false }]

type anf_binding = Let of string * string list * aexpr
[@@deriving show { with_path = false }]

type anf_prog = anf_binding list
