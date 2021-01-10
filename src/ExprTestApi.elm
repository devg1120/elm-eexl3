module ExprTestApi exposing (..)

import Expr exposing (..)
import Array
import Dict

import Parser exposing (..)

---------------------------------------------------------------------
getStringIndex : String -> Int -> Int -> Int -> Int -> String
getStringIndex mstr r c r_ c_ =
     if  r < 1 || c < 1 then
         "N/A row & col not negative"
     else if r < r_ then
         "N/A over col"
     else if r == r_ && c == c_ then
         String.left 1 mstr
     else
        let
          char = String.left 1 mstr
          (r_2 , c_2 )= if char == "\n" then
                           (r_ + 1,   1)
                        else
                           (r_ , c_ + 1)


          mstr2 = String.dropLeft 1 mstr 
        in 
        if mstr2 == "" then
           "N/A over row"
        else
           getStringIndex mstr2 r c r_2 c_2

stringIndex : String -> Int -> Int -> String
stringIndex str_ r c =
        getStringIndex str_ r c 1 1


errAnalysis : String -> List DeadEnd -> String
errAnalysis source err =
            let
               h = List.head err
               --c = case  problem of
               p = case  h of
                     Just h_ ->
                         let
                            c = String.fromInt h_.col
                            r = String.fromInt h_.row
                            sc = stringIndex source h_.row h_.col 
                         in
                         case h_.problem of
                              Expecting s ->
                                "UnExpect " ++ r ++ ":" ++ c ++ " -> " ++ sc 
                              ExpectingInt ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingHex ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingOctal ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingBinary ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingFloat ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingNumber ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingVariable ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingSymbol s ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingKeyword s ->
                                "unExpect:" ++ r ++ "-" ++ c
                              ExpectingEnd ->
                                "unExpect:" ++ r ++ "-" ++ c
                              UnexpectedChar ->
                                "unExpect:" ++ r ++ "-" ++ c
                              Problem s ->
                                "unExpect:" ++ r ++ "-" ++ c
                              BadRepeat ->
                                "unExpect:" ++ r ++ "-" ++ c

                     Nothing ->
                         --Problem "nothing"
                         "nothing"
            in
            p

----------------------------------------------------------------
test_exprexec : String -> List (String, OutVal) 
                        -> List (String, List String) 
                        -> List (String, Context -> Input -> OutVal)
                        -> ExprResult String OutVal (String, (Array.Array ArgValue))
test_exprexec source add_const add_enum add_func =
    let
        ast =
            parse source

        result =
            case ast of
                Err err ->
                    ExprErr (errAnalysis source err )

                Ok expr ->
                    let
                        reg_const entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addConstant name value context_

                        reg_enum  entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addEnumDict name value context_

                        reg_func entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addFunction name value context_


                        context_empty = empty
                        context1 = List.foldl reg_const context_empty add_const
                        context2 = List.foldl reg_enum  context1      add_enum
                        context  = List.foldl reg_func  context2      add_func

                        userenv =
                            { userFunctions = Dict.empty
                            , userEnv = Dict.empty
                            }

                        userfunc : userenv -> Context -> String -> Array.Array ArgValue -> OutVal
                        userfunc userenv_ context_ funcname input_args =
                            OString "OK"

                        ans =
                            evaluate userenv userfunc context expr
                    in
                    --Debug.toString ans
                    ans
    in
    result

test_contextdump :  List (String, OutVal) 
                     -> List (String, List String) 
                     -> List (String, Context -> Input -> OutVal)
                     -> Context
test_contextdump add_const add_enum add_func =
    let
                        reg_const entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addConstant name value context_

                        reg_enum  entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addEnumDict name value context_

                        reg_func entry_ context_ =
                              let
                                name    = Tuple.first entry_
                                value   = Tuple.second entry_
                              in
                              addFunction name value context_


                        context_empty = empty
                        context1 = List.foldl reg_const context_empty add_const
                        context2 = List.foldl reg_enum  context1      add_enum
                        context  = List.foldl reg_func  context2      add_func

                        userenv =
                            { userFunctions = Dict.empty
                            , userEnv = Dict.empty
                            }

                        userfunc : userenv -> Context -> String -> Array.Array ArgValue -> OutVal
                        userfunc userenv_ context_ funcname input_args =
                            OString "OK"

    in
    context

test_check expr result constlist enumlist funclist =
     let
      r_ = test_exprexec expr constlist enumlist funclist
     in
      case r_ of
            ExprOk value ->
                     if result == value then
                        "OK"
                     else
                        "ExprOK ERR " ++ (Debug.toString value)
            ExprErr err ->
                     if result == (OString err) then
                        "Er"
                     else
                        "ExprErr ERR " ++ (Debug.toString err)

            ExprNotFoundFunc notfound ->
                        "ExprNotFoundFunc"

test  constlist enumlist funclist testlist =
      let
          testfunc entry_  =
                 let
                    expr    = Tuple.first entry_
                    result_ = Tuple.second entry_
                 in
                 test_check expr result_ constlist enumlist funclist
       in
       List.map testfunc testlist   

----------------------------------------


