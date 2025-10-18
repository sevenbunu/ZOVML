(* Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* open Ast
open Machine

type codegen_ctx =
  { var_env : (string * reg) list
  ; code : instr list
  ; free_regs : reg list
  ; used_regs : reg list
  ; stack_offset : int
  }

let fresh_temp ctx =
  match ctx.free_regs with
  | [] -> failwith "Out of temporary registers"
  | reg :: rest ->
    let new_ctx = { ctx with free_regs = rest; used_regs = reg :: ctx.used_regs } in
    reg, new_ctx
;;

let emit ctx instr = { ctx with code = instr :: ctx.code }

let lookup_var ctx (Ident name) =
  try List.assoc name ctx.var_env with
  | Not_found -> failwith ("Unknown variable: " ^ name)
;;

let rec codegen_expr ctx (expr, _tp) target_reg =
  match expr with
  | Identificator name ->
    let var_reg = lookup_var ctx name in
    emit ctx (Mv (target_reg, var_reg))
  | Const const ->
    (match const with
     | Int n -> emit ctx (Li (target_reg, n))
     | _ -> failwith "Unsupported constant")
  | Neg expr ->
    let temp, ctx = fresh_temp ctx in
    let ctx = codegen_expr ctx expr temp in
    let zero, ctx = fresh_temp ctx in
    let ctx = emit ctx (Li (zero, 0)) in
    emit ctx (Sub (target_reg, zero, temp))
  | Let (let_def, body) -> codegen_let ctx let_def body target_reg
  | IfThenEsle (cond, then_expr, else_expr) ->
    (* If с метками и переходами *)
    let cond_reg, ctx = fresh_temp ctx in
    let then_label, ctx = fresh_label ctx "then" in
    let else_label, ctx = fresh_label ctx "else" in
    let end_label, ctx = fresh_label ctx "end_if" in
    (* Вычисляем условие *)
    let ctx = codegen_expr ctx cond cond_reg in
    (* Если условие == 0, идем в else *)
    let zero, ctx = fresh_temp ctx in
    let ctx = emit ctx (Li (zero, 0)) in
    let ctx = emit ctx (Beq (cond_reg, zero, else_label)) in
    (* Then-ветка *)
    let ctx = emit ctx (Label then_label) in
    let ctx = codegen_expr ctx then_expr target_reg in
    (* Безусловный переход в конец - эмулируем через beq *)
    let ctx = emit ctx (Beq (zero, zero, end_label)) in
    (* Else-ветка *)
    let ctx = emit ctx (Label else_label) in
    let ctx =
      match else_expr with
      | Some expr -> codegen_expr ctx expr target_reg
      | None -> emit ctx (Li (target_reg, 0))
    in
    emit ctx (Label end_label)
  | FunctionApply (func, arg1, args) ->
    (* Вызов функции С ПОДДЕРЖКОЙ РЕКУРСИИ! *)
    let all_args = arg1 :: args in
    (* Получаем имя функции *)
    let func_name =
      match func with
      | Identificator name, _ -> name
      | _ -> failwith "Only direct function calls supported"
    in
    (* Сохраняем используемые регистры в стек *)
    let ctx = emit ctx (Comment "Save registers to stack") in
    let ctx = emit ctx (Addi (SP, SP, -16)) in
    let ctx = emit ctx (Sd (RA, 8, SP)) in
    let ctx =
      List.fold_left
        (fun ctx (i, arg) ->
           let arg_reg = A i in
           codegen_expr ctx arg arg_reg)
        ctx
        (List.mapi (fun i a -> i, a) all_args)
    in
    let ctx = emit ctx (Call func_name) in
    let ctx = emit ctx (Comment "Restore registers from stack") in
    let ctx = emit ctx (Ld (Ra, 8, Sp)) in
    let ctx = emit ctx (Addi (Sp, Sp, 16)) in
    if target_reg <> A0 then emit ctx (Mv (target_reg, A0)) else ctx
  | Binop (left, op, right) ->
    let temp1, ctx = fresh_temp ctx in
    let temp2, ctx = fresh_temp ctx in
    let ctx = codegen_expr ctx left temp1 in
    let ctx = codegen_expr ctx right temp2 in
    (match op with
     | Eq ->
       let diff, ctx = fresh_temp ctx in
       let zero, ctx = fresh_temp ctx in
       let one, ctx = fresh_temp ctx in
       let ctx = emit ctx (Sub (diff, temp1, temp2)) in
       let ctx = emit ctx (Li (zero, 0)) in
       let ctx = emit ctx (Li (one, 1)) in
       let ctx = emit ctx (Slt (target_reg, zero, diff)) in
       emit ctx (Sub (target_reg, one, target_reg))
     | Ne ->
       let diff, ctx = fresh_temp ctx in
       let zero, ctx = fresh_temp ctx in
       let ctx = emit ctx (Sub (diff, temp1, temp2)) in
       let ctx = emit ctx (Li (zero, 0)) in
       emit ctx (Slt (target_reg, zero, diff))
     | Lt -> emit ctx (Slt (target_reg, temp1, temp2))
     | Le ->
       let temp, ctx = fresh_temp ctx in
       let one, ctx = fresh_temp ctx in
       let ctx = emit ctx (Slt (temp, temp2, temp1)) in
       let ctx = emit ctx (Li (one, 1)) in
       emit ctx (Sub (target_reg, one, temp))
     | Gt -> emit ctx (Slt (target_reg, temp2, temp1))
     | Ge ->
       let temp, ctx = fresh_temp ctx in
       let one, ctx = fresh_temp ctx in
       let ctx = emit ctx (Slt (temp, temp1, temp2)) in
       let ctx = emit ctx (Li (one, 1)) in
       emit ctx (Sub (target_reg, one, temp))
     | Add -> emit ctx (Add (target_reg, temp1, temp2))
     | Sub -> emit ctx (Sub (target_reg, temp1, temp2))
     | Mul -> emit ctx (Mul (target_reg, temp1, temp2))
     | Div -> emit ctx (Div (target_reg, temp1, temp2))
     | _ -> failwith "Unsupported operation")
  | _ -> failwith "Unsupported expression"

and codegen_let ctx let_def body target_reg =
  match let_def with
  | Let_simple (is_rec, pattern, value_expr) ->
    (match pattern with
     | PVar var_name ->
       (match value_expr with
        | Fun (param, func_body), _ ->
          codegen_function ctx var_name param func_body body target_reg
        | _ ->
          let var_reg, ctx = fresh_temp ctx in
          let ctx = codegen_expr ctx value_expr var_reg in
          let old_env = save_env ctx in
          let ctx = bind_var ctx var_name var_reg in
          let ctx = codegen_expr ctx body target_reg in
          restore_env ctx old_env)
     | _ -> failwith "Only simple patterns supported")

and codegen_function ctx func_name param func_body body target_reg =
  let func_label = func_name in
  let end_label, ctx = fresh_label ctx ("end_" ^ func_name) in
  let ctx = emit ctx (Beq (Zero, Zero, end_label)) in
  let ctx = emit ctx (Label func_label) in
  let ctx = emit ctx (Comment ("Function: " ^ func_name)) in
  let old_env = save_env ctx in
  let ctx = bind_var ctx param (A 0) in
  let ctx = codegen_expr ctx func_body (A 0) in
  let ctx = emit ctx Ret in
  let ctx = restore_env ctx old_env in
  let ctx = emit ctx (Label end_label) in
  let func_reg, ctx = fresh_temp ctx in
  let ctx = bind_var ctx func_name func_reg in
  codegen_expr ctx body target_reg
;; *)
