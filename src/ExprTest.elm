module ExprTest exposing (..)

import Expr exposing (..)
import Array
import Dict


---------------------------------------------------------------------


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


exprexec2 : String -> ExprResult String OutVal (String, (Array.Array ArgValue))
exprexec2 str =
    let
        ast =
            parse str

        result =
            case ast of
                Err err ->
                    --Debug.toString err
                    --ExprErr "AST Error" (OString "err")
                    ExprErr "AST Error" 

                Ok expr ->
                    let
                        context =
                            empty
                                |> addConstant "test1" (OString "OKOK")
                                |> addConstant "abc" (OString "_ABCD_")
                                |> addConstant "regex_test" (OString "123 ABC aas qdd A987 SDDFGG A666")
                                |> addConstant "regex_test2" (OString " ABC aas qdd A SDDFGG A")
                                |> addConstant "findstr1" (OString "A(BC)")
                                |> addConstant "findstr2" (OString "A(\\d\\d\\d)")
                                |> addConstant "findstr3" (OString "(\\d\\d\\d)")
                                |> addConstant "test_float" (OFloat 10.1)
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
                                |> addConstant "array_string_test"
                                    (OArray
                                        (Array.fromList
                                            [ OString "a__"
                                            , OString "b__"
                                            , OString "c__"
                                            , OString "d__"
                                            , OString "e__"
                                            , OString "f__"
                                            ]
                                        )
                                    )
                                |> addConstant "index" (OFloat 1)
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
                    --Debug.toString ans
                    ans
    in
    result


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
    ( " 1 + 3 + (7 // 2)   "  ,  OFloat 7          )
   ,( " 1 + 3 + (7 /  2)   "  ,  OFloat 7.5        )
   ,( " 1 + 3 + (7 %  2)   "  ,  OFloat 5          )
   ,( " 1 + 3 + (6 /  2)   "  ,  OFloat 7          )
   ,( " 1 - 3 + (6 /  2)   "  ,  OFloat 1          )
   ,( " 1 - 5 + (6 /  2)   "  ,  OFloat -1         )
   ,( " 1 - 5 + (-6 / 2)   "  ,  OFloat -7         )
   ,( " -1 + 5             "  ,  OFloat 4          )
   ,( " (-1) + 5           "  ,  OFloat 4          )
   ,( " -2 * 5             "  ,  OFloat -10        )
   ,( " -2 * -5            "  ,  OFloat 10         )


   ,( " False              "  ,  OBool False       )
   ,( " True               "  ,  OBool True        )
   ,( " True && True       "  ,  OBool True        )
   ,( " True && False      "  ,  OBool False       )
   ,( " True || True       "  ,  OBool True        )
   ,( " True || False      "  ,  OBool True        )

   ,( " \"abc\" + \"ABC\"  "  ,  OString "abcABC"  )
   ,( " \"abc\" + test1    "  ,  OString "abcOKOK" )
   ,( " 1.1 + test_float   "  ,  OFloat 11.2       )
   ,( " -1.1 + test_float  "  ,  OFloat 9          )
   ,( " -1 + test_float    "  ,  OFloat  9.1       )
   ,( " \"abc\" + strjoin( \"ABC\", \"XYZ\") " , OString "abcABCXYZ"  )
   ,( " \"abc\" + strjoin( \"ABC\", test1)   " , OString "abcABCOKOK" )

   ,( " \"xyz\" == \"xyz\" "  ,  OBool True    )
   ,( " \"xyz\" == \"xyZ\" "  ,  OBool False    )
   ,( " \"xyz\" != \"xyz\" "  ,  OBool False    )
   ,( " \"xyz\" != \"xyZ\" "  ,  OBool True    )

   ,( " test1   == \"OKOK\" "  ,  OBool True    )
   ,( " \"OKOK\" == test1   "  ,  OBool True    )
   ,( " test1   != \"OKOK\" "  ,  OBool False    )
   ,( " \"OKOK\" != test1   "  ,  OBool False    )

   ,( " 1.0 <= 100.1       "  ,  OBool True    )
   ,( " 1.0 <  100.1       "  ,  OBool True    )
   ,( " 1.0 >  100.1       "  ,  OBool False    )
   ,( " 1.0 >= 100.1       "  ,  OBool False    )
   ,( " 1.0 == 100.1       "  ,  OBool False    )
   ,( " 1.0 != 100.1       "  ,  OBool True     )
   ,( " 1.1 == 1.1         "  ,  OBool True     )
   ,( " e                  "  ,  OString "dicGetSerch...not found:e" )
   --,(  ,    )
   ]

test_list2 = Array.fromList [  -- array 
    ( " [ 1,2,3,4,5] " ,
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"1\",\"2\",\"3\",\"4\",\"5\"]  ", 
        OArray (Array.fromList [OString "1",OString "2",OString "3",OString "4",OString "5"])   )

   ,( " array_test  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[1]  ", 
        OFloat 2   )

   ,( " array_string_test[3]  ", 
        OString "d__"   )

   ,( " [ 1,2,3] + [4,5] " ,
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"a\", \"b\"] + [\"c\", \"d\"] " ,
        OArray (Array.fromList [OString "a", OString "b", OString "c", OString "d"])  )

   ,( " [ \"a\", \"b\"] + [\"c\", \"d\"] + array_string_test" ,
        OArray (Array.fromList [OString "a", OString "b", OString "c", OString "d" ,OString "a__",OString "b__",OString "c__",OString "d__",OString "e__",OString "f__"])  )
   ]

test_list3 = Array.fromList [  -- array slice
     -- [start:end]	start から end - 1 まで
     -- [start:]	start から最後尾まで
     -- [:end]	        先頭から end - 1 まで
     -- [:]	        先頭から最後尾まで

    ( " array_test  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[0]  ", 
        OFloat 1   )

   ,( " array_test[1]  ", 
        OFloat 2   )

   ,( " array_test[4]  ", 
        OFloat 5   )

   ,( " array_test[index + 1]  ", 
        OFloat 3   )

   ,( " array_test[9]  ", 
        OString " !!not_found arrayIndex"   )

   ,( " array_test[0:3]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])   )

   ,( " array_test[2:4]  ", 
        OArray (Array.fromList [OFloat 3,OFloat 4])   )

   ,( " array_test[1:-1]  ", 
        OArray (Array.fromList [OFloat 2,OFloat 3,OFloat 4])   )

   ,( " array_test[-2:5]  ", 
        OArray (Array.fromList [OFloat 4,OFloat 5])   )

   ,( " array_test[:2]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2])   )

   ,( " array_test[2:]  ", 
        OArray (Array.fromList [OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_string_test[:2]  ", 
        OArray (Array.fromList [OString "a__",OString "b__"])   )

   ,( " array_string_test[2:]  ", 
        OArray (Array.fromList [OString "c__",OString "d__",OString "e__", OString "f__"])   )


   ]

test_list4 = Array.fromList [  -- dict
    ( " {\"ab\" : 1, \"xy\" : 2}   ", 
        ODict (Dict.fromList [("ab",OFloat 1),("xy",OFloat 2)])   )

   ,( " dict_test.c   ", 
        OFloat 3   )

   ,( " dict_test{\"c\"}  ", 
        OFloat 3   )
   ]

findstr1 = "A(BC)"
findstr2 = "A(\\d\\d\\d)"

test_list5 = Array.fromList [  -- variable method
    ( "  abc.sub(1,2)  ", 
        OString "AB"   )

   ,( "  abc.len()  ", 
        OFloat 6   )

   ,( "  array_test.len()  ", 
        OFloat 5   )

   ,( "  abc.match(\"ABC\")  ", 
        OBool True   )

                                -- "regex_test" (OString "123 ABC aas qdd A987 SDDFGG A666")
                                -- "findstr1"   (OString "A(BC)")
                                -- "findstr2"   (OString "A(\\d\\d\\d)")
                                -- "findstr3"   (OString "(\\d\\d\\d)")

   ,( "  regex_test.find(\"A(BC)\")  ", 
        OArray (Array.fromList [OString "BC"])   )

   ,( "  regex_test.find(\"A(XY)\")  ", 
        OArray (Array.fromList [])   )

   ,( "  regex_test.find(findstr1)  ", 
        OArray (Array.fromList [OString "BC"])   )

   ,( "  regex_test.find(findstr2)  ", 
       OArray (Array.fromList [OString "987",OString "666"])   )

   ,( "  regex_test.find(findstr3)  ", 
       OArray (Array.fromList [OString "123",OString "987",OString "666"])   )

   ,( "  regex_test.isContain(\"[0-9]\")  ", 
        OBool True   )

   ,( "  regex_test2.isContain(\"[0-9]\")  ", 
        OBool False   )

   ,( "  regex_test.replace(\"[0-9]\",\"-\")  ", 
        OString "--- ABC aas qdd A--- SDDFGG A---"   )

   ]

r1 = Array.map testfunc test_list1
r2 = Array.map testfunc test_list2
r3 = Array.map testfunc test_list3
r4 = Array.map testfunc test_list4
r5 = Array.map testfunc test_list5


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
