(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type reg =
  | Zero
  | RA
  | SP
  | T of int
  | S of int
  | A of int
  | ROffset of reg * int

let rec pp_reg ppf =
  let open Format in
  function
  | Zero -> fprintf ppf "zero"
  | RA -> fprintf ppf "ra"
  | SP -> fprintf ppf "sp"
  | A x -> fprintf ppf "a%d" x
  | S x -> fprintf ppf "s%d" x
  | T x -> fprintf ppf "t%d" x
  | ROffset (r, 0) -> fprintf ppf "(%a)" pp_reg r
  | ROffset (r, n) -> fprintf ppf "%d(%a)" n pp_reg r
;;

type instr =
  | Add of reg * reg * reg
  | Addi of reg * reg * int
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Li of reg * int
  | Ld of reg * int * reg
  | Sd of reg * int * reg
  | Slt of reg * reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | Ble of reg * reg * string
  | Ecall
  | Ret
  | Call of string
  | Mv of reg * reg
  | Label of string
  | Comment of string

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, n) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Add (r1, r2, r3) -> fprintf ppf "add  %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Sub (r1, r2, r3) -> fprintf ppf "sub %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Mul (r1, r2, r3) -> fprintf ppf "mul %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Div (r1, r2, r3) -> fprintf ppf "div %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Slt (r1, r2, r3) -> fprintf ppf "slt %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Li (r, n) -> fprintf ppf "li %a, %d" pp_reg r n
  | Ecall -> fprintf ppf "ecall"
  | Ret -> fprintf ppf "ret"
  | Ld (r1, offset, r2) -> fprintf ppf "ld %a, (%d)%a" pp_reg r1 offset pp_reg r2
  | Mv (rd, rs) -> fprintf ppf "mv %a, %a" pp_reg rd pp_reg rs
  | Sd (r1, offset, r2) -> fprintf ppf "sd %a, %d(%a)" pp_reg r1 offset pp_reg r2
  | Beq (r1, r2, offset) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Blt (r1, r2, offset) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Ble (r1, r2, offset) -> fprintf ppf "ble %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Label s -> fprintf ppf "%s:" s
  | Comment s -> fprintf ppf "# %s" s
  | Call symbol -> fprintf ppf "call %s" symbol
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k r1 r2 r3 = k @@ Add (r1, r2, r3)
let sub k r1 r2 r3 = k @@ Sub (r1, r2, r3)
let mul k r1 r2 r3 = k @@ Mul (r1, r2, r3)
let li k r n = k (Li (r, n))
let ecall k = k Ecall
let ret k = k Ret
let ld k a b offset = k (Ld (a, b, offset))
let sd k a b offset = k (Sd (a, b, offset))
let mv k a b = k (Mv (a, b))
let beq k r1 r2 r3 = k @@ Beq (r1, r2, r3)
let blt k r1 r2 r3 = k @@ Blt (r1, r2, r3)
let ble k r1 r2 r3 = k @@ Ble (r1, r2, r3)
let label k s = k (Label s)
let comment k s = k (Comment s)
