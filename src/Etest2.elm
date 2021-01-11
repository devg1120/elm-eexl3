module Etest2 exposing (..)


import Array
import Stack
import Dict
import Exec exposing (..)
import Expr exposing (..)
import Stdlib exposing (..)
import Debug 

import Test.Test01 
import Test.Test02 

-------------------------------------
test script_name =
    let
        ast =
            stmt_parse script_name

        result =
            case ast of
                Err err ->
                    Debug.toString err

                Ok stmts ->
                    let
                        context =
                            empty
                                |> addFunction "strjoin" strjoin

                        --context = init_context
                        userenv =
                            userenvEmpty

                        ( userenv_2, ans ) =
                            eval userenv context stmts

                        ans2 =
                            case ans of
                                Context a ->
                                    a

                        env =
                            Debug.toString userenv_2

                        ans3 =
                            ans2.constants

                        log =
                            ans2.log

                        --ans4 = Debug.toString  (Dict.toList ans3)
                        --           |> String.replace "\"" ""
                        --           |> String.replace "OFloat" ""
                        --           |> String.replace "[" ""
                        --           |> String.replace "]" ""
                        --           |> String.replace "),(" ") ("
                        --ans4 = Debug.toString  (ans3)
                        ans4 =
                            Debug.toString ans2
                                |> String.replace "\"" ""
                                |> String.replace "OFloat" ""
                                |> String.replace "[" ""
                                |> String.replace "]" ""
                                |> String.replace "," " ="
                                |> String.replace ") =(" " , "

                        --|> String.replace "(" ""
                        --|> String.replace ")" ""
                        --|> String.replace "Stack Dict.fromList " ""
                    in
                    ans4 ++ " -> " ++ log ++ "[" ++ env
    in
    result


-------------------------------------
test1 script_name =
    let
        ast = stmt_parse script_name
    in
     case ast of
       Err err ->
           (Err err)
       Ok stmts ->
           let
               context = empty
                       |> addFunction "strjoin" strjoin
               userenv = userenvEmpty
               ( userenv_2, ans ) =
                   eval userenv context stmts
               ans2 =
                   case ans of
                       Context a ->
                           a
               list = case (Stack.top ans2.constants) of
                     Just a ->
                         Dict.toList a
                     _ ->
                         []
           in
           (Ok list)


test2 script_name =
    let
        ast = stmt_parse script_name
    in
     case ast of
         Err err ->
             (Err err)
         Ok stmts ->
             let
                 context = empty
                       |> addFunction "strjoin" strjoin
                 userenv = userenvEmpty
                 ( userenv_2, ans ) =
                     eval userenv context stmts
                 ans2 =
                     case ans of
                         Context a ->
                             a
             in
             (Ok ans2.log)


test3 script_name =    -- output: Userenv
    let
       ast = stmt_parse script_name
    in
       case ast of
           Err err ->
               (Err err)
           Ok stmts ->
               let
                   context = empty
                       |> addFunction "strjoin" strjoin
                   userenv = userenvEmpty
                   ( userenv_2, context_2 ) =
                       eval userenv context stmts
               in
               (Ok userenv_2)

test4 script_name =    -- output: Context
    let
       ast = stmt_parse script_name
    in
       case ast of
           Err err ->
               (Err err)
           Ok stmts ->
               let
                   context = empty
                       |> addFunction "strjoin" strjoin
                   userenv = userenvEmpty
                   ( userenv_2, context_2 ) =
                       eval userenv context stmts
               in
               (Ok context_2)
------------------------------------------------------


r1 = test Test.Test01.script
r2 = test Test.Test02.script

p1 = test1 Test.Test01.script -- constant
p2 = test2 Test.Test01.script -- log
p3 = test3 Test.Test01.script -- env
p4 = test4 Test.Test01.script -- context

