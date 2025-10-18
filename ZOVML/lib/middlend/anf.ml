(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ZOVML.Ast
open Anf_ast
open Utils
open Base

type var_postfix =
  | VarApply
  | VarIte
  | VarTuple

let gen_var_prefix = function
  | VarIte -> "__anf_varite"
  | VarApply -> "__anf_varapply"
  | VarTuple -> "__anf_vartuple"
;;

module COUNTERMONAD = struct
  include BASEMONAD
end

open COUNTERMONAD

let gen_num =
  let* i = read in
  let* _ = write (i + 1) in
  return i
;;

let rec gen_varname varkind st =
  let* new_num = gen_num in
  let name = Base.String.concat [ gen_var_prefix varkind; Base.Int.to_string new_num ] in
  if Base.Set.mem st name then gen_varname varkind st else return name
;;

let retrieve_exprs lst = List.map ~f:(fun (e, _) -> e) lst

let rec anf env expr cont =
  let anf_multiple env exprs cont =
    let rec helper acc = function
      | [] -> cont (List.rev acc)
      | h :: tl -> anf env h (fun i -> helper (i :: acc) tl)
    in
    helper [] exprs
  in
  match expr with
  | Const const ->
    let imm_const =
      match const with
      | Bool b -> ImmBool b
      | Int i -> ImmInt i
      | Unit -> ImmUnit
    in
    cont imm_const
  | Identificator (Ident id) -> cont (ImmIdentifier id)
  | Tuple ((e1, _), (e2, _), elast) ->
    let elast = List.map ~f:(fun (e, _) -> e) elast in
    anf_multiple env (e1 :: e2 :: elast) (fun list ->
      let* new_name = gen_varname VarTuple env in
      let imm_id = ImmIdentifier new_name in
      let* aexp = cont imm_id in
      return (ALetIn (new_name, CImmExpr (ImmTuple list), aexp)))
  | IfThenEsle ((i, _), (t, _), e_opt) ->
    anf env i (fun cn ->
      let* new_name = gen_varname VarIte env in
      let imm_ident = ImmIdentifier new_name in
      let* aexp = cont imm_ident in
      let* then_branch = anf env t (fun imm_then -> return (AExpr (CImmExpr imm_then))) in
      let* else_branch =
        match e_opt with
        | TSome (e, _) -> anf env e (fun imm_then -> return (AExpr (CImmExpr imm_then)))
        | TNone -> return (AExpr (CImmExpr ImmUnit))
      in
      return (ALetIn (new_name, CIfThenElse (cn, then_branch, else_branch), aexp)))
  | FunctionApply ((e1, _), (e2, _), etl) ->
    let expr, etl = e1, e2 :: retrieve_exprs etl in
    anf env expr (fun imm_exp ->
      anf_multiple env etl (fun imm_etl ->
        let* new_name = gen_varname VarApply env in
        let imm_id = ImmIdentifier new_name in
        let* aexp = cont imm_id in
        let build_app imm_expr = function
          | h :: tl -> return (CApplication (imm_expr, h, tl))
          | [] -> fail "Application ANF failed"
        in
        build_app imm_exp imm_etl >>= fun e -> return (ALetIn (new_name, e, aexp))))
  | Let (Let_simple (_, (_, pat, _), (cex, _)), (out, _)) ->
    (match pat with
     | PIdentificator (Ident id) ->
       let new_env = Set.add (Set.empty (module String)) id in
       anf new_env out (fun imm_out ->
         anf new_env cex cont >>= fun e -> return (ALetIn (id, CImmExpr imm_out, e)))
     | _ -> fail "let ANF FAILED")
  | _ -> fail "Error while trying to build aexpr while ANF"
;;

let run_anf exp binded =
  let rec set s tl =
    match tl with
    | [] -> s
    | hd :: tl -> set (Base.Set.union s (Set.add (Set.empty (module String)) hd)) tl
  in
  run
    (anf
       (set (Set.empty (module String)) binded)
       exp
       (fun ie -> return (AExpr (CImmExpr ie))))
    0
;;
