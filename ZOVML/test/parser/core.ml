(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let pp_result printer = function
  | Result.Ok res -> Format.printf "%a" printer res
  | Result.Error s -> Format.eprintf "Error%s" s
;;

let param_parse parser str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
;;
