(** Copyright 2025, Alexei Dmitrievtsev, Konstantin Oreshin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Middlend
open ZOVML.Ast

let pp_result printer = function
  | Result.Ok res -> Format.printf "%a" printer res
  | Result.Error s -> Format.eprintf "Error%s" s
;;

let param_parse parser str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
;;

open Middlend.Anf

let test_aexpr ast =
  match run_anf ast with
  | _, Result.Ok ast -> Format.printf "%s" (Print_anf.print_aexpr ast)
  | _, Result.Error err -> Format.printf "Error: %s" err
;;

let test_ful bind =
  match run_anf_top bind with
  | _, Result.Ok ast -> Format.printf "%s" (Print_anf.print_bindings ast)
  | _, Result.Error err -> Format.printf "Error: %s" err
;;

let%expect_test "let fn = (arg1 arg2 arg3, arg4 arg5, arg6, arg7)" =
  test_ful
    [ Def
        (FunDef
           ( Nonrec
           , Ident ""
           , ([], PIdentificator (Ident "fn"), [])
           , []
           , ( Tuple
                 ( ( FunctionApply
                       ( (Identificator (Ident "arg1"), [])
                       , (Identificator (Ident "arg2"), [])
                       , [ Identificator (Ident "arg3"), [] ] )
                   , [] )
                 , ( FunctionApply
                       ( (Identificator (Ident "arg4"), [])
                       , (Identificator (Ident "arg5"), [])
                       , [] )
                   , [] )
                 , [ Identificator (Ident "arg6"), []; Identificator (Ident "arg7"), [] ]
                 )
             , [] ) ))
    ];
  [%expect
    {|
    let fn  = let __anf_varapply0 = arg1 arg2 arg3 in
    let __anf_varapply1 = arg4 arg5  in
    let __anf_vartuple2 = (__anf_varapply0, __anf_varapply1, arg6, arg7) in
    __anf_vartuple2;;
    |}]
;;

let%expect_test "let fn x = if x > 2 then x - 1 else x + 2" =
  test_ful
    [ Def
        (FunDef
           ( Nonrec
           , Ident ""
           , ([], PIdentificator (Ident "fn"), [])
           , [ [], PIdentificator (Ident "x"), [] ]
           , ( IfThenEsle
                 ( ( FunctionApply
                       ( (Identificator (Ident "( > )"), [])
                       , (Identificator (Ident "x"), [])
                       , [ Const (Int 2), [] ] )
                   , [] )
                 , ( FunctionApply
                       ( (Identificator (Ident "( - )"), [])
                       , (Identificator (Ident "x"), [])
                       , [ Const (Int 1), [] ] )
                   , [] )
                 , TSome
                     ( FunctionApply
                         ( (Identificator (Ident "( + )"), [])
                         , (Identificator (Ident "x"), [])
                         , [ Const (Int 2), [] ] )
                     , [] ) )
             , [] ) ))
    ];
  [%expect
    {|
    let fn x = let __anf_varapply0 = ( > ) x 2 in
    let __anf_varite1 = if __anf_varapply0 then let __anf_varapply2 = ( - ) x 1 in
    __anf_varapply2 else let __anf_varapply3 = ( + ) x 2 in
    __anf_varapply3 in
    __anf_varite1;;
    |}]
;;

let%expect_test "let fn x = if x > 2 then x - 1 else if x < 1 then x - 1" =
  test_ful
    [ Def
        (FunDef
           ( Nonrec
           , Ident ""
           , ([], PIdentificator (Ident "fn"), [])
           , [ [], PIdentificator (Ident "x"), [] ]
           , ( IfThenEsle
                 ( ( FunctionApply
                       ( (Identificator (Ident "( > )"), [])
                       , (Identificator (Ident "x"), [])
                       , [ Const (Int 2), [] ] )
                   , [] )
                 , ( FunctionApply
                       ( (Identificator (Ident "( - )"), [])
                       , (Identificator (Ident "x"), [])
                       , [ Const (Int 1), [] ] )
                   , [] )
                 , TSome
                     ( IfThenEsle
                         ( ( FunctionApply
                               ( (Identificator (Ident "( < )"), [])
                               , (Identificator (Ident "x"), [])
                               , [ Const (Int 1), [] ] )
                           , [] )
                         , ( FunctionApply
                               ( (Identificator (Ident "( - )"), [])
                               , (Identificator (Ident "x"), [])
                               , [ Const (Int 1), [] ] )
                           , [] )
                         , TSome
                             ( FunctionApply
                                 ( (Identificator (Ident "( + )"), [])
                                 , (Identificator (Ident "x"), [])
                                 , [ Const (Int 2), [] ] )
                             , [] ) )
                     , [] ) )
             , [] ) ))
    ];
  [%expect
    {|
    let fn x = let __anf_varapply0 = ( > ) x 2 in
    let __anf_varite1 = if __anf_varapply0 then let __anf_varapply2 = ( - ) x 1 in
    __anf_varapply2 else let __anf_varapply3 = ( < ) x 1 in
    let __anf_varite4 = if __anf_varapply3 then let __anf_varapply5 = ( - ) x 1 in
    __anf_varapply5 else let __anf_varapply6 = ( + ) x 2 in
    __anf_varapply6 in
    __anf_varite4 in
    __anf_varite1;;
    |}]
;;

let%expect_test "let fn a = if a > 2 then a - 1 else a + 2" =
  test_ful
    [ Def
        (FunDef
           ( Nonrec
           , Ident ""
           , ([], PIdentificator (Ident "fn"), [])
           , [ [], PIdentificator (Ident "a"), [] ]
           , ( IfThenEsle
                 ( ( FunctionApply
                       ( (Identificator (Ident "( > )"), [])
                       , (Identificator (Ident "a"), [])
                       , [ Const (Int 2), [] ] )
                   , [] )
                 , ( FunctionApply
                       ( (Identificator (Ident "( - )"), [])
                       , (Identificator (Ident "a"), [])
                       , [ Const (Int 1), [] ] )
                   , [] )
                 , TSome
                     ( FunctionApply
                         ( (Identificator (Ident "( + )"), [])
                         , (Identificator (Ident "a"), [])
                         , [ Const (Int 2), [] ] )
                     , [] ) )
             , [] ) ))
    ];
  [%expect
    {|
    let fn a = let __anf_varapply0 = ( > ) a 2 in
    let __anf_varite1 = if __anf_varapply0 then let __anf_varapply2 = ( - ) a 1 in
    __anf_varapply2 else let __anf_varapply3 = ( + ) a 2 in
    __anf_varapply3 in
    __anf_varite1;;
    |}]
;;

let%expect_test "let fn a b = if a > 2 then u a fn2 b else a + c" =
  test_ful
    [ Def
        (FunDef
           ( Nonrec
           , Ident ""
           , ([], PIdentificator (Ident "fn"), [])
           , [ [], PIdentificator (Ident "a"), [] ]
           , ( IfThenEsle
                 ( ( FunctionApply
                       ( (Identificator (Ident "( > )"), [])
                       , (Identificator (Ident "a"), [])
                       , [ Const (Int 2), [] ] )
                   , [] )
                 , ( FunctionApply
                       ( (Identificator (Ident "u"), [])
                       , (Identificator (Ident "a"), [])
                       , [ Identificator (Ident "fn2"), []
                         ; Identificator (Ident "b"), []
                         ] )
                   , [] )
                 , TSome
                     ( FunctionApply
                         ( (Identificator (Ident "( + )"), [])
                         , (Identificator (Ident "a"), [])
                         , [ Identificator (Ident "c"), [] ] )
                     , [] ) )
             , [] ) ))
    ];
  [%expect
    {|
    let fn a = let __anf_varapply0 = ( > ) a 2 in
    let __anf_varite1 = if __anf_varapply0 then let __anf_varapply2 = u a fn2 b in
    __anf_varapply2 else let __anf_varapply3 = ( + ) a c in
    __anf_varapply3 in
    __anf_varite1;;
    |}]
;;
