module ExprTest exposing (..)

import Expr exposing (..)
import Array
import Dict


---------------------------------------------------------------------
{--

test_strjoin : Context -> Input -> OutVal
test_strjoin context ar =
    let
        a_ =
            case Array.get 0 ar of
                Just (AvString a) ->
                    a

                Just (AvVar a) ->
                    let
                        value =
                            getConstant a context

                        ans_ =
                            case value of
                                Just v ->
                                    v

                                _ ->
                                    OString " AvVar not_found"

                        result =
                            case ans_ of
                                OString v ->
                                    v

                                _ ->
                                    " AvVar not_found"
                    in
                    result

                _ ->
                    ""

        b_ =
            case Array.get 1 ar of
                Just (AvString a) ->
                    a

                Just (AvVar a) ->
                    let
                        value =
                            getConstant a context

                        ans_ =
                            case value of
                                Just v ->
                                    v

                                _ ->
                                    OString " AvVar not_found"

                        result =
                            case ans_ of
                                OString v ->
                                    v

                                _ ->
                                    " AvVar not_found"
                    in
                    result

                _ ->
                    ""

        ans =
            a_ ++ b_
    in
    OString ans


exprexec : String -> String
exprexec str =
    let
        ast =
            parse str

        result =
            case ast of
                Err err ->
                    Debug.toString err

                Ok expr ->
                    let
                        context =
                            empty
                                |> addConstant "test1" (OString "OKOK")
                                |> addConstant "test_flort" (OFloat 10.1)
                                |> addConstant "array_test"
                                    (OArray
                                        (Array.fromList
                                            [ OFloat 1
                                            , OFloat 2
                                            , OFloat 3
                                            , OFloat 4
                                            , OFloat 5
                                            ]
                                        )
                                    )
                                |> addConstant "dict_test"
                                    (ODict
                                        (Dict.fromList
                                            [ ( "a", OFloat 1 )
                                            , ( "b", OFloat 2 )
                                            , ( "c", OFloat 3 )
                                            , ( "d", OFloat 4 )
                                            , ( "e", OFloat 5 )
                                            ]
                                        )
                                    )
                                |> addFunction "strjoin" test_strjoin

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
                    Debug.toString ans
    in
    result

--}

test_check expr result =
     let
      r_ = exprexec2 expr
     in
      case r_ of
            ExprOk value ->
                     if result == value then
                        "OK"
                     else
                        "ERR " ++ (Debug.toString value)
            ExprErr err ->
                        "ExprErr"
            ExprNotFoundFunc notfound ->
                        "ExprNotFoundFunc"
                     
testfunc entry_  =
       let
          expr    = Tuple.first entry_
          result_ = Tuple.second entry_
       in
       test_check expr result_
----------------------------------------
{--
test expr result_ arr =
       Array.push (test_check expr result_) arr

r1 = exprexec " 1 + 3 + (7 //2) "
r2 = test_check " 1 + 3 + (7 //2) " (OFloat 7)

rlt0 = Array.fromList [] 
       |> Array.push (test_check " 1 + 3 + (7 //2) " (OFloat 7) ) 

rlt2 = Array.fromList [] 
       |> test " 1 + 3 + (7 // 2) " (OFloat 7)
       |> test " 1 + 3 + (7 /  2) " (OFloat 7.5)
       |> test " 1 + 3 + (7 %  2) " (OFloat 5)
       |> test " \"abc\" + \"ABC\" " (OString "abcABC" )
--}
----------------------------------------

test_list1 = Array.fromList [ 
    ( " 1 + 3 + (7 // 2) "  ,  OFloat 7          )
   ,( " 1 + 3 + (7 /  2) "  ,  OFloat 7.5        )
   ,( " 1 + 3 + (7 %  2) "  ,  OFloat 5          )
   ,( " \"abc\" + \"ABC\" " ,  OString "abcABC"  )
   ,( " False             " ,  OBool False       )
   ,( " True              " ,  OBool True        )
   ,( " True && True      " ,  OBool True        )
   ,( " True && False     " ,  OBool False       )
   ,( " True || True      " ,  OBool True        )
   ,( " True || False     " ,  OBool True        )
   ,( " \"abc\" + test1   " ,  OString "abcOKOK" )
   ,( " 1.1 + test_float  " ,  OFloat 11.2       )
   ,( " \"abc\" + strjoin( \"ABC\", \"XYZ\") " , OString "abcABCXYZ"  )
   ,( " \"abc\" + strjoin( \"ABC\", test1)   " , OString "abcABCOKOK" )
   ,( " 1.0 <= 100.1                         " , OBool True    )
   ,( " 1.0 <  100.1                         " , OBool True    )
   ,( " 1.0 >  100.1                         " , OBool False    )
   ,( " 1.0 >= 100.1                         " , OBool False    )
   ,( " 1.0 == 100.1                         " , OBool False    )
   ,( " 1.0 != 100.1                         " , OBool True     )
   ,( " 1.1 == 1.1                           " , OBool True     )
   ,( " e                                    " , OString "dicGetSerch...not found:e" )
   --,(  ,    )
   ]

test_list2 = Array.fromList [  -- array
    ( " [ 1,2,3,4,5] " ,
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"1\",\"2\",\"3\",\"4\",\"5\"]  ", 
        OArray (Array.fromList [OString "1",OString "2",OString "3",OString "4",OString "5"])   )

   ,( " array_test  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[0]  ", 
        OFloat 1   )

   ]

test_list3 = Array.fromList [  -- dict
    ( " {\"ab\" : 1, \"xy\" : 2}   ", 
        ODict (Dict.fromList [("ab",OFloat 1),("xy",OFloat 2)])   )

   ,( " dict_test.c   ", 
        OFloat 3   )

   ,( " dict_test{\"c\"}  ", 
        OFloat 3   )
   ]


r1 = Array.map testfunc test_list1
r2 = Array.map testfunc test_list2
r3 = Array.map testfunc test_list3


-----------------------------------------------------
{--
> import Expr exposing (..)
> exprexec " 1 + 3 + (7 //2) "
"OFloat 7" : String
> exprexec " 1 + 3 + (7 /2) "
"OFloat 7.5" : String
> exprexec " 1 + 3 + (7 %2) "
"OFloat 5" : String
> exprexec " \"abc\" + \"ABC\" "
"OString \"abcABC\"" : String
> exprexec " False "
"OBool False" : String
> exprexec " True "
"OBool True" : String
> exprexec " True && True"
"OBool True" : String
> exprexec " True && False"
"OBool False" : String
> exprexec " True || True"
"OBool True" : String
> exprexec " True || False"
"OBool True" : String
> exprexec " \"abc\" + test1 "
"OString \"abcOKOK\"" : String

> exprexec " 1.1  + test_float "
"OFloat 11.2" : String
> exprexec " \"abc\" + strjoin( \"ABC\", \"XYZ\") "
"OString \"abcABCXYZ\"" : Strin
> exprexec " \"abc\" + strjoin( \"ABC\", test1) "
"OString \"abcABCOKOK\"" : String

> exprexec "1.0 <= 100.1"
"OBool True" : String
> exprexec "1.0 < 100.1"
"OBool True" : String
> exprexec "1.0 > 100.1"
"OBool False" : String
> exprexec "1.0 >= 100.1"
"OBool False" : String
> exprexec "1.0 <= 100.1"
"OBool True" : String
> exprexec "1.0 == 100.1"
"OBool False" : String
> exprexec "1.0 != 100.1"
"OBool True" : String
> exprexec "1.1 == 1.1"
"OBool True" : String

> exprexec "e"
"OString \"not_found\"" : String

                                --array
> exprexec " [ 1,2,3,4,5] "
"OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])" : String

> exprexec " [ \"1\",\"2\",\"3\",\"4\",\"5\"] "

                                --dict
> exprexec "{\"ab\" : 1, \"xy\" : 2}"
"ODict (Dict.fromList [(\"ab\",OFloat 1),(\"xy\",OFloat 2)])" : String


> exprexec " array_test " 
"OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])"
    : String
> exprexec " dict_test " 
"ODict (Dict.fromList [(\"a\",OFloat 1),(\"b\",OFloat 2),(\"c\",OFloat 3),(\"d\",OFloat 4),(\"e\",OFloat 5)])"

> exprexec "array_test[0]"
"OFloat 1" : String

> exprexec "dict_test.c"
"OFloat 3" : String

> exprexec "dict_test{\"c\"}"
"OFloat 3" : String

--}
