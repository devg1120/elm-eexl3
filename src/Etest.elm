module Etest exposing (..)

import Exec exposing(..)
import Expr exposing(..)
import Stdlib exposing(..)

--import Parser exposing (..)

import Array


--exec3 script_name =
test script_name =
  let
     --ast = run script script_name
     ast = stmt_parse script_name
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                             |> addFunction "strjoin" strjoin 
                 --context = init_context

                 userenv = userenvEmpty
                 (userenv_2,ans) = eval userenv context stmts

                 ans2 = case ans of
                            Context a -> 
                                     a

                 env = Debug.toString  userenv_2

                 ans3 =  ans2.constants 
                 log =  ans2.log
                 --ans4 = Debug.toString  (Dict.toList ans3)
                 --           |> String.replace "\"" ""
                 --           |> String.replace "OFloat" ""
                 --           |> String.replace "[" ""
                 --           |> String.replace "]" ""
                 --           |> String.replace "),(" ") ("
                 
                 --ans4 = Debug.toString  (ans3)
                 ans4 = Debug.toString  (ans2)
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

------------------------------------------------------
script3 = """
  let a = 1;
  let b = 0;
  let c = 0;
  fn test ( a, b, c) {
     a = a + b + c;
     return a;
  }
  fn str ( l, r ) {
     return strjoin(l, r) ;
  }
  if a > 1 {
     b = 1;
  } else {
     let b = 2;
     c = 1;
  }
   a = 100;
  c = strjoin("ABC", "abc"); //lib
  let e1 = "abc";
  let e2 = "ABC";
  let e = strjoin(e1, e2); //lib
  let d = test(1,2,3);       //user func
  let ss = str("xyz", "1XYZ");
  return a;
"""
