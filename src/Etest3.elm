module Etest3 exposing (..)


import Array
import Stack
import Dict
import Exec exposing (..)
import Expr exposing (..)
import Stdlib exposing (..)
import Debug 
import Parser

import Test.Test01 
import Test.Test02 
import Test.Test03 
import Test.Test04 

-------------------------------------
type TestResult 
   = TestConstant (Result (List Parser.DeadEnd) (List ( String, OutVal)))
   | TestLog (Result (List Parser.DeadEnd) String)
   | TestUserEnv (Result (List Parser.DeadEnd) UserEnv)
   | TestContext (Result (List Parser.DeadEnd) Context)
   | TestErr (List Parser.DeadEnd)

type TestMode
   = M_Constant
   | M_Env
   | M_Log
   | M_Context


testx mode script_name =
    let
     ast = stmt_parse script_name
     result = case ast of
       Err err ->
           (Err err)
       Ok stmts ->
           let
               --context = empty
               context = empty
                       |> addFunction "strjoin" strjoin
               userenv = userenvEmpty
               ans  =
                   eval userenv context stmts
            in
            (Ok ans)
    in
    case mode of
       M_Constant ->
           case result of
             Ok r2 ->
                let
                  (userenv , context) = r2
                  context2 =
                      case context of
                          Context a ->
                              a
                  list = case (Stack.top context2.constants) of
                        Just a ->
                            Dict.toList a
                        _ ->
                            []
                in
                TestConstant (Ok list)
             Err  str ->
                TestErr  str

       M_Log ->
           case result of
             Ok r2 ->
                let
                  (userenv , context) = r2
                  context2 =
                      case context of
                          Context a ->
                              a
                in
                TestLog (Ok context2.log)
             Err  str ->
                TestErr  str

       M_Env ->
           case result of
             Ok r2 ->
                let
                  (userenv , context) = r2
                in
                TestUserEnv (Ok userenv)
             Err  str ->
                TestErr  str

       M_Context ->
           case result of
             Ok r2 ->
                let
                  (userenv , context) = r2
                in
                TestContext (Ok context)
             Err  str ->
                TestErr  str

------------------------------------------------------


a1 = testx M_Constant Test.Test01.script
a2 = testx M_Log      Test.Test01.script
a3 = testx M_Env      Test.Test01.script
a4 = testx M_Context  Test.Test01.script

b1 = testx M_Constant Test.Test02.script
b2 = testx M_Log      Test.Test02.script
b3 = testx M_Env      Test.Test02.script
b4 = testx M_Context  Test.Test02.script

c1 = testx M_Constant Test.Test03.script
c2 = testx M_Log      Test.Test03.script
c3 = testx M_Env      Test.Test03.script
c4 = testx M_Context  Test.Test03.script

d1 = testx M_Constant Test.Test04.script
d2 = testx M_Log      Test.Test04.script
d3 = testx M_Env      Test.Test04.script
d4 = testx M_Context  Test.Test04.script

