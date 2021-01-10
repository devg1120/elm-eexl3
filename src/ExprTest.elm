module ExprTest exposing (..)

import Expr exposing (..)
import Array
import Dict

import Parser exposing (..)
import ExprTestApi exposing (..)

---------------------------------------------------------------------


typeof : Context -> Input -> OutVal
typeof context ar =
            case Array.get 0 ar of
                Just (AvString a) ->
                    OString "AvString"

                Just (AvVar a) ->
                    OString "AvVar"

                _ ->
                    OString "don't know"


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

------------------------------------------------------------------------------------
test_func = [
       ("strjoin"  ,test_strjoin )
      ,("typeof"   ,typeof       )
   ]

------------------------------------------------------------------------------------
test_enum =  [ 
      ("Sushi"
         ,["Tako","Ika","Tai","Tamago","Maguro"]
      )
     ,("Car"
         ,["Honda","Toyota","Suzuki","Nissan","Matsuda"]
      )
     ,("City"
         ,["Tokyo","Osaka","Kobe","Fukuoka","Nara"]
      )
   ]

test_const =  [ 

  ( "test1"           ,(OString "OKOK") )
 ,( "abc"             ,(OString "_ABCD_") )
 ,( "regex_test"      ,(OString "123 ABC aas qdd A987 SDDFGG A666"))
 ,( "regex_test2"     ,(OString " ABC aas qdd A SDDFGG A"))
 ,( "split_test"      ,(OString "tom , 99, 90, 85 "))
 ,( "findstr1"        ,(OString "A(BC)"))
 ,( "findstr2"        ,(OString "A(\\d\\d\\d)"))
 ,( "findstr3"        ,(OString "(\\d\\d\\d)"))
 ,( "test_float"      ,(OFloat 10.1))
 ,( "test_bool"       ,(OBool  False))
 ,( "test_enum"       ,(OEnum (3,3)))
 ,( "array_test"      ,
                                    (OArray
                                        (Array.fromList
                                            [ OFloat 1
                                            , OFloat 2
                                            , OFloat 3
                                            , OFloat 4
                                            , OFloat 5
                                            ]
                                        )
                                    ))
 ,( "array_test2d"    ,
                                    (OArray
                                        (Array.fromList
                                            [ OArray (Array.fromList [OFloat 1,OFloat 2])
                                            , OArray (Array.fromList [OFloat 3,OFloat 4])
                                            , OArray (Array.fromList [OFloat 5,OFloat 6])
                                            ]
                                        )
                                    ))
 ,( "array_test2d_1"  ,
                                    (OArray
                                        (Array.fromList
                                            [ OArray (Array.fromList [OFloat 1,OFloat 2])
                                            , OArray (Array.fromList [OFloat 3,OFloat 4])
                                            ]
                                        )
                                    ))
 ,( "array_test2d_2"  ,
                                    (OArray
                                        (Array.fromList
                                            [ OArray (Array.fromList [OFloat 5,OFloat 6])
                                            , OArray (Array.fromList [OFloat 7,OFloat 8])
                                            ]
                                        )
                                    ))
 ,( "array_string_test" ,
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
                                    ))
 ,( "array_dict_test"  ,
                                    (OArray
                                        (Array.fromList
                                            [ODict  (Dict.fromList
                                                        [ ("a", OFloat 1)
                                                        , ("b", OFloat 2)
                                                        , ("c", OFloat 3)
                                                        ]
                                                    )
                                            ,ODict  (Dict.fromList
                                                        [ ("a", OFloat 4)
                                                        , ("b", OFloat 5)
                                                        , ("c", OFloat 6)
                                                        ]
                                                    )
                                            ]
                                        )
                                    ))
 ,( "array_dict_test2"  ,
                                    (OArray
                                        (Array.fromList
                                            [ODict  (Dict.fromList
                                                        [ ("a", OFloat 1)
                                                        , ("b", OFloat 2)
                                                        , ("c", OFloat 3)
                                                        ]
                                                    )
                                            ,ODict  (Dict.fromList
                                                        [ ("a", OFloat 4)
                                                        , ("b", OFloat 5)
                                                        , ("c", OFloat 6)
                                                        ]
                                                    )
                                            ,ODict  (Dict.fromList
                                                        [ ("a", OFloat 7)
                                                        , ("b", OFloat 8)
                                                        , ("c", OFloat 9)
                                                        ]
                                                    )
                                            ]
                                        )
                                    ))
 ,( "index"         ,(OFloat 1))
 ,( "dict_index"    ,(OString "c"))
 ,( "dict_index2"   ,(OString "xy"))
 ,( "dict_test"     ,
                                    (ODict
                                        (Dict.fromList
                                            [ ( "a", OFloat 1 )
                                            , ( "b", OFloat 2 )
                                            , ( "c", OFloat 3 )
                                            , ( "d", OFloat 4 )
                                            , ( "e", OFloat 5 )
                                            ]
                                        )
                                    ))
 ,( "dict_test2"   ,
                                    (ODict
                                        (Dict.fromList
                                            [ ( "ab", OArray
                                                       (Array.fromList
                                                                 [ OFloat 1
                                                                 , OFloat 2
                                                                 , OFloat 3
                                                                 ]
                                                        )
                                              )
                                            , ( "xy", OArray
                                                       (Array.fromList
                                                                 [ OFloat 4
                                                                 , OFloat 5
                                                                 , OFloat 6
                                                                 ]
                                                        )
                                              )
                                            ]
                                        )
                                    ))
 ]

------------------------------------------------------------------------------------

test_list1 =  [ 

    ( " 1 + 3 + (7 // 2)   "  ,  OFloat 7            )
   ,( """ 1 + 3  
             + (7 // 2)  """  ,  OFloat 7            )
   ,( " 1 + 3 + (7 /  2)   "  ,  OFloat 7.5          )
   ,( " 1 + 3 + (7 %  2)   "  ,  OFloat 5            )
   ,( " 1 + 3 + (6 /  2)   "  ,  OFloat 7            )
   ,( " 1 - 3 + (6 /  2)   "  ,  OFloat 1            )
   ,( " 1 - 5 + (6 /  2)   "  ,  OFloat -1           )
   ,( " 1 - 5 + (6 // 2)   "  ,  OFloat -1           )
   ,( " 1 - 5 + (-6 / 2)   "  ,  OFloat -7           )
   ,( " -1 + 5             "  ,  OFloat 4            )
   ,( " (-1) + 5           "  ,  OFloat 4            )
   ,( " -2 *  5            "  ,  OFloat -10          )
   ,( "  2 * -5            "  ,  OFloat -10          )
   ,( " -2 * -5            "  ,  OFloat 10           )
   ,( "( -2 * -5 )         "  ,  OFloat 10           )


   ,( " False              "  ,  OBool False         )
   ,( " True               "  ,  OBool True          )
   ,( " True && True       "  ,  OBool True          )
   ,( " True && False      "  ,  OBool False         )
   ,( " False && False     "  ,  OBool False         )
   ,( " False && test_bool "  ,  OBool False         )
   ,( " True || True       "  ,  OBool True          )
   ,( " True || False      "  ,  OBool True          )
   ,( " False || False     "  ,  OBool False         )

   ,( " \"abc\" + \"ABC\"  "  ,  OString "abcABC"    )
   ,( " \"abc\" + test1    "  ,  OString "abcOKOK"   )

   ,( " test_float         "  ,  OFloat 10.1         )
   ,( " 1.1  + test_float  "  ,  OFloat 11.2         )
   ,( " -1.1 + test_float  "  ,  OFloat 9            )
   ,( " -1   + test_float  "  ,  OFloat  9.1         )

   ,( " test1                "  ,  OString "OKOK"    )
   ,( " strjoin(\"ABC\", \"XYZ\") "            , OString "ABCXYZ"     )
   ,( " \"abc\" + strjoin( \"ABC\", \"XYZ\") " , OString "abcABCXYZ"  )
   ,( " \"abc\" + strjoin( \"ABC\", test1)   " , OString "abcABCOKOK" )

   ,( " \"xyz\" == \"xyz\"   "  ,  OBool True        )
   ,( " \"_ABCD_\" == abc    "  ,  OBool True        )
   ,( " \"123\" == \"123\"   "  ,  OBool True        )
   ,( " \"xyz\" == \"xyZ\"   "  ,  OBool False       )
   ,( " \"xyz\" != \"xyz\"   "  ,  OBool False       )
   ,( " \"xyz\" != \"xyZ\"   "  ,  OBool True        )

   ,( " test1                "  ,  OString "OKOK"    )
   ,( " test1    == \"OKOK\" "  ,  OBool True        )
   ,( " \"OKOK\" == test1    "  ,  OBool True        )
   ,( " test1    != \"OKOK\" "  ,  OBool False       )
   ,( " \"OKOK\" != test1    "  ,  OBool False       )

   ,( " 1.0 <= 100.1         "  ,  OBool True        )
   ,( " 1.0 <  100.1         "  ,  OBool True        )
   ,( " 1.0 >  100.1         "  ,  OBool False       )
   ,( " 1.0 >= 100.1         "  ,  OBool False       )
   ,( " 1.0 == 100.1         "  ,  OBool False       )
   ,( " 1.0 != 100.1         "  ,  OBool True        )
   ,( " 1.1 == 1.1           "  ,  OBool True        )

   ,( " True == True         "  ,  OBool True        )
   ,( " True != True         "  ,  OBool False       )
   ,( " True == False         "  ,  OBool False      )
   ,( " True != False         "  ,  OBool True       )

   ,( " 1   <= 100.1         "  ,  OBool True        )
   ,( " 1   <  100.1         "  ,  OBool True        )
   ,( " 1   >  100.1         "  ,  OBool False       )
   ,( " 1   >= 100.1         "  ,  OBool False       )
   ,( " 1   == 100.1         "  ,  OBool False       )
   ,( " 1   != 100.1         "  ,  OBool True        )
   ,( " 1   == 1.1           "  ,  OBool False       )

   ,( " 1.0 <= 100           "  ,  OBool True        )
   ,( " 1.0 <  100           "  ,  OBool True        )
   ,( " 1.0 >  100           "  ,  OBool False       )
   ,( " 1.0 >= 100           "  ,  OBool False       )
   ,( " 1.0 == 100           "  ,  OBool False       )
   ,( " 1.0 != 100           "  ,  OBool True        )
   ,( " 1.1 == 1             "  ,  OBool False       )

   ,( " 1   <= 100           "  ,  OBool True        )
   ,( " 1   <  100           "  ,  OBool True        )
   ,( " 1   >  100           "  ,  OBool False       )
   ,( " 1   >= 100           "  ,  OBool False       )
   ,( " 1   == 100           "  ,  OBool False       )
   ,( " 1   != 100           "  ,  OBool True        )
   ,( " 1   == 1             "  ,  OBool True        )

   ,( " City::Kobe           "  ,  OEnum (3,3)       )
   ,( " Sushi::Tai           "  ,  OEnum (1,3)       )
   ,( " Car::Matsuda         "  ,  OEnum (2,5)       )
--   ,( " Car::Mary            "  ,  OEnum (2,5)       )
   ,( " City::Kobe == City::Kobe   "  ,  OBool True  )
   ,( " City::Kobe != City::Kobe   "  ,  OBool False )
   ,( " City::Kobe != City::Nara   "  ,  OBool True  )
   ,( " City::Kobe == test_enum    "  ,  OBool True  )
   ,( " test_enum  == City::Nara   "  ,  OBool False )

   ,( " e                    "  ,  OString "NotFound1:e" )
   --,(  ,    )

   ]

test_list1err =  [ 

    ( " 1 +- 3 + (7 // 2)       "  ,  OString "UnExpect 1:5 -> -"         )
   ,( """ 1 + 3  
            ++ (7 // 2)  """       ,  OString "UnExpect 2:14 -> +"        )
   ,( " --1 + 5                 "  ,  OString "UnExpect 1:2 -> -"         )
   ,( " -2 ** 5                 "  ,  OString "UnExpect 1:6 -> *"         )
   ,( "  -4                     "  ,  OFloat -4                           )
   ,( "  -4  3 5                "  ,  OFloat -4                           )
   ,( "  -4+ 3 5                "  ,  OFloat -1                           )
   ,( "  -4 !! 5                "  ,  OFloat -4                           )
   ,( "  -4 ++ 5                "  ,  OString "UnExpect 1:7 -> +"         )
   ,( " false                   "  ,  OString "NotFound1:false"            )
   ,( " True /  True            "  ,  OString "div value must Float"      )
   ,( " True //  True           "  ,  OString "div2 value must Float"     )
   ,( " True %  True            "  ,  OString "div3 value must Float"     )
   ,( " True *  True            "  ,  OString "* value must Float"        )
   ,( " True -  True            "  ,  OString "- value must Float"        )
   ,( " True +  True            "  ,  OString "+ value must Float/String/Array"  )
   ,( " \"abc\" - \"ABC\"       "  ,  OString "- value must Float"               )
   ,( " \"abc\" - test1         "  ,  OString "- value must Float"               )
   ,( " \"xyz\"  + test_float   "  ,  OString "+ value must Float/String/Array"  )
   ,( " \"xyz\" == 1            "  ,  OString "== UnMatch Type Float/String/Bool")
   ,( " \"xyz\" == abc1         "  ,  OString "== UnMatch Type Float/String/Bool")

   ,( " 1.0 <= \"TEST\"         "  ,  OString "<= UnMatch Type Float"     )
   ,( " 1.0 <= abc1             "  ,  OString "<= UnMatch Type Float"     )
   ,( " 1.0 <  \"TEXT\"         "  ,  OString "<  UnMatch Type Float"     )
   ,( " 1.0 >  \"TEXT\"         "  ,  OString ">  UnMatch Type Float"     )
   ,( " 1.0 >= \"TEXT\"         "  ,  OString ">= UnMatch Type Float"     )
   ,( " 1.0 == \"TEXT\"         "  ,  OString "== UnMatch Type Float/String/Bool"   )
   ,( " 1.0 != \"TEXT\"         "  ,  OString "!= UnMatch Type Float/String/Bool"   )

   ,( "  4  + xx                "  ,  OString "+ b NotFound:xx"           )
   ,( "  zz + 4                 "  ,  OString "a + NotFound:zz"           )
   ,( "  -4 - xx                "  ,  OString "- b NotFound:xx"           )
   ,( "  zz - 4                 "  ,  OString "a - NotFound:zz"           )
   ,( "  -4 * xx                "  ,  OString "* b NotFound:xx"           )
   ,( "  zz * 4                 "  ,  OString "a * NotFound:zz"           )
   ,( "  -4 / xx                "  ,  OString "/ b NotFound:xx"           )
   ,( "  zz / 4                 "  ,  OString "a / NotFound:zz"           )
   ,( "  -4 // xx               "  ,  OString "// b NotFound:xx"          )
   ,( "  zz // 4                "  ,  OString "a // NotFound:zz"          )
   ,( "  -4 % xx                "  ,  OString "% b NotFound:xx"           )
   ,( "  zz % 4                 "  ,  OString "a % NotFound:zz"           )
   ,( "  True && xx             "  ,  OString "&& b NotFound:xx"          )
   ,( "  zz && True             "  ,  OString "a && NotFound:zz"          )
   ,( "  True || xx             "  ,  OString "|| b NotFound:xx"          )
   ,( "  zz || True             "  ,  OString "a || NotFound:zz"          )
   ,( "  1 && xx                "  ,  OString "&& b NotFound:xx"          )
   ,( "  zz && 1                "  ,  OString "a && NotFound:zz"          )
   ,( "  1 || xx                "  ,  OString "|| b NotFound:xx"          )
   ,( "  zz || 1                "  ,  OString "a || NotFound:zz"          )
 
   ,( "  1 && test_bool         "  ,  OString "&& value must Bool"        )
   ,( "  test_bool && 1         "  ,  OString "&& value must Bool"        )
   ,( "  1 || test_bool         "  ,  OString "|| value must Bool"        )
   ,( "  test_bool || 1         "  ,  OString "|| value must Bool"        )

   ,( "   -1                    "  ,  OFloat -1        )
   ,( "   -index                "  ,  OString "UnExpect 1:4 -> -"         )
   ,( " Car::Mary            "  ,  OString "enum not entry:Car::Maty"       )

   --,(  ,    )
   ]


---------------------------------------------------------------------
test_list2 =  [  -- array 

    ( " [ 1,2,3,4,5] " ,
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [] " ,
        OArray (Array.fromList [])  )

   ,( " [ test1,2,3,4,5] " ,
        OArray (Array.fromList [OString "OKOK" ,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"+\" + \"_\",2,3,4,5] " ,
        OArray (Array.fromList [OString "+_" ,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ test1 + \"_\",2,3,4,5+1] " ,
        OArray (Array.fromList [OString "OKOK_" ,OFloat 2,OFloat 3,OFloat 4,OFloat 6])  )

   ,( " [ test1 + abc + \"_\",2,3,4,5] " ,
        OArray (Array.fromList [OString "OKOK_ABCD__" ,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"1\",\"2\",\"3\",\"4\",\"5\"]  ", 
        OArray (Array.fromList [OString "1",OString "2",OString "3",OString "4",OString "5"])   )

   ,( " array_test  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[1]  ", 
        OFloat 2   )

   ,( " array_string_test  ", 
        OArray (Array.fromList [OString "a__",OString "b__",OString "c__",OString "d__",OString "e__",OString "f__"])   )

   ,( " array_string_test[3]  ", 
        OString "d__"   )

   ,( " [ 1,2,3] + [4,5] " ,
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])  )

   ,( " [ \"a\", \"b\"] + [\"c\", \"d\"] " ,
        OArray (Array.fromList [OString "a", OString "b", OString "c", OString "d"])  )

   ,( " [ \"a\", \"b\"] + [\"c\", \"d\"] + array_string_test" ,
        OArray (Array.fromList [OString "a", OString "b", OString "c", OString "d" ,OString "a__",OString "b__",OString "c__",OString "d__",OString "e__",OString "f__"])  )

   ,( " array_string_test.sub(1,2)  ", 
        OArray (Array.fromList [OString "b__",OString "c__"])   )

    -------------------------------------------------------2d array
   ,( " [ [1,2],[3,4],[5,6]] " ,
        OArray (Array.fromList [OArray (Array.fromList [OFloat 1,OFloat 2]),
                                OArray (Array.fromList [OFloat 3,OFloat 4]),
                                OArray (Array.fromList [OFloat 5,OFloat 6])])    )

   ,( " [ [1,2],[],[5,6]] " ,
        OArray (Array.fromList [OArray (Array.fromList [OFloat 1,OFloat 2]),
                                OArray (Array.fromList []),
                                OArray (Array.fromList [OFloat 5,OFloat 6])])    )

   ,( " [ [test1,2],[3,4],[5,6]] " ,
        OArray (Array.fromList [OArray (Array.fromList [OString "OKOK",OFloat 2]),
                                OArray (Array.fromList [OFloat 3,OFloat 4]),
                                OArray (Array.fromList [OFloat 5,OFloat 6])])    )

   ,( " [ [test1 + \"ok\",2],[3,4],[5,6+2]] " ,
        OArray (Array.fromList [OArray (Array.fromList [OString "OKOKok",OFloat 2]),
                                OArray (Array.fromList [OFloat 3,OFloat 4]),
                                OArray (Array.fromList [OFloat 5,OFloat 8])])    )


   ,( " [ [\"1\",\"2\"],[3,4],[5,6]] " ,
        OArray (Array.fromList [OArray (Array.fromList [OString "1",OString "2"]),
                                OArray (Array.fromList [OFloat 3,OFloat 4]),
                                OArray (Array.fromList [OFloat 5,OFloat 6])])    )

   ,( " array_test2d  ", 
        OArray (Array.fromList [OArray (Array.fromList [OFloat 1,OFloat 2]),
                                OArray (Array.fromList [OFloat 3,OFloat 4]),
                                OArray (Array.fromList [OFloat 5,OFloat 6])])    )

   ,( " array_test2d[1]  ", 
                                OArray (Array.fromList [OFloat 3,OFloat 4])      )

   ,( " array_test2d[1][1]  ", 
                                OFloat 4      )

   ,( " array_dict_test  ",   
        OArray (Array.fromList 
             [ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2),("c",OFloat 3)])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ]) )

   ,( " array_dict_test[0] ", 
             ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2),("c",OFloat 3)])
              )

   ,( " array_dict_test[1] ", 
             ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
              )

   ,( " array_dict_test[1].c ", 
             OFloat 6 )
              
   ,( " array_dict_test[1]{\"c\"} ", 
             OFloat 6 )

   ,( " array_dict_test[index]{dict_index} ", 
             OFloat 6 )

   ,( " array_dict_test2  ",   
        OArray (Array.fromList 
             [ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2),("c",OFloat 3)])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ,ODict (Dict.fromList [("a",OFloat 7),("b",OFloat 8),("c",OFloat 9)])
             ]) )

   ,( " [{\"a\" : 1, \"b\" : 2, \"c\" :3 }, {\"a\" : 4, \"b\" : 5, \"c\" :6 },{\"a\" : 7, \"b\" : 8, \"c\" :9 }] ", 
        OArray (Array.fromList 
             [ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2),("c",OFloat 3)])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ,ODict (Dict.fromList [("a",OFloat 7),("b",OFloat 8),("c",OFloat 9)])
             ]) )

   ,( " [{\"a\" : test1, \"b\" : 2+1, \"c\" :3 }, {\"a\" : 4, \"b\" : 5, \"c\" :6 },{\"a\" : 7, \"b\" : 8, \"c\" :9 }] ", 
        OArray (Array.fromList 
             [ODict (Dict.fromList [("a",OString "OKOK"),("b",OFloat 3),("c",OFloat 3)])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ,ODict (Dict.fromList [("a",OFloat 7),("b",OFloat 8),("c",OFloat 9)])
             ]) )

   ,( " [{\"a\" : test1, \"b\" : 2+1, \"c\" :[1,2,3] }, {\"a\" : 4, \"b\" : 5, \"c\" :6 },{\"a\" : 7, \"b\" : 8, \"c\" :9 }] ", 
        OArray (Array.fromList 
             [ODict (Dict.fromList [("a",OString "OKOK"),("b",OFloat 3),
                                    ("c",OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3]))])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ,ODict (Dict.fromList [("a",OFloat 7),("b",OFloat 8),("c",OFloat 9)])
             ]) )

   ,( " [{}, {\"a\" : 4, \"b\" : 5, \"c\" :6 },{\"a\" : 7, \"b\" : 8, \"c\" :9 }] ", 
        OArray (Array.fromList 
             [ODict (Dict.fromList [])
             ,ODict (Dict.fromList [("a",OFloat 4),("b",OFloat 5),("c",OFloat 6)])
             ,ODict (Dict.fromList [("a",OFloat 7),("b",OFloat 8),("c",OFloat 9)])
             ]) )
   ]

test_list2err =  [  -- array 
    ( " array_test  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[1]  ", 
        OFloat 2   )

   ,( " array_test[5]  ", 
        OString "out of index:5"  )

   ,( " array_test[-5]  ", 
        OString "out of index:-5"  )

   ,( " array_test[index]  ",   -- index = 1
        OFloat 2   )

   ,( " array_test[index + 1]  ",   -- index = 1
        OFloat 3   )

   ,( " array_test[ind ]  ",   
        OString "out of index:NotFound:ind"   )

   ,( " array_test[ind + 1 ]  ",   
        OString "out of index:a + NotFound:ind"   )

   ,( " array_test[1 + ind  ]  ",   
        OString "out of index:+ b NotFound:ind"   )

   ,( " array_test[\"X\"]  ",   
        OString "out of index: must Int/Float"   )

   ,( " array_test[abc]  ",   
        OString "out of index: must Int/Float"   )

   ,( " array_test[True]  ",   
        OString "out of index: must Int/Float"   )

   ,( " array_test[]  ",   
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )


   ]

test_list3 =  [  -- array slice
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
        OString "out of index:9"   )

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

test_list3err =  [  -- array slice
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
        OString "out of index:9"   )

   ,( " array_test[0:3]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])   )

   ,( " array_test[2:4]  ", 
        OArray (Array.fromList [OFloat 3,OFloat 4])   )

   ,( " array_test[1:-1]  ", 
        OArray (Array.fromList [OFloat 2,OFloat 3,OFloat 4])   )

   ,( " array_test[-2:5]  ", 
        OArray (Array.fromList [OFloat 4,OFloat 5])   )

----------------------------------------------------------------------
   ,( " array_test[:0]  ", 
        OArray (Array.fromList [])   )

   ,( " array_test[:1]  ", 
        OArray (Array.fromList [OFloat 1])   )

   ,( " array_test[:2]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2])   )

   ,( " array_test[:5]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[:6]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[:7]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[:-1]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4])   )

   ,( " array_test[:-2]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])   )

   ,( " array_test[:-4]  ", 
        OArray (Array.fromList [OFloat 1])   )

   ,( " array_test[:-5]  ", 
        OArray (Array.fromList [])   )

   ,( " array_test[:-6]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4])   )

   ,( " array_test[:-7]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])   )

----------------------------------------------------------------------
   ,( " array_test[0:]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[1:]  ", 
        OArray (Array.fromList [OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[2:]  ",
        OArray (Array.fromList [OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[5:]  ",
        OArray (Array.fromList [])   )

   ,( " array_test[6:]  ",
        OArray (Array.fromList [])   )

   ,( " array_test[7:]  ",
        OArray (Array.fromList [])   )

   ,( " array_test[-1:]  ",
        OArray (Array.fromList [OFloat 5])   )

   ,( " array_test[-2:]  ",
        OArray (Array.fromList [OFloat 4,OFloat 5])   )

   ,( " array_test[-7:]  ",
        OArray (Array.fromList [OFloat 4,OFloat 5])   )

----------------------------------------------------------------------
   ,( " array_test[0:0]  ", 
        OArray (Array.fromList [])   )

   ,( " array_test[:]  ", 
        OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])   )

   ,( " array_test[1:0]  ", 
        OArray (Array.fromList [])   )

   ,( " array_test[0:1]  ", 
        OArray (Array.fromList [OFloat 1])   )

   ,( " array_test[1:2]  ", 
        OArray (Array.fromList [OFloat 2])   )

   ,( " array_test[1:3]  ", 
        OArray (Array.fromList [OFloat 2,OFloat 3])   )

   ,( " array_test[index:3]  ", 
        OArray (Array.fromList [OFloat 2,OFloat 3])   )

   ,( " array_test[0:index]  ", 
        OArray (Array.fromList [OFloat 1])   )

   ,( " array_test[ind:3]  ", 
        OString "NotFound:ind"   )

   ,( " array_test[0:ind]  ", 
        OString "NotFound:ind"   )

   ,( " undef[0:ind]  ", 
        OString "NotFound array:undef"   )
----------------------------------------------------------------------
{--
   ,( " array_string_test[:2]  ", 
        OArray (Array.fromList [OString "a__",OString "b__"])   )

   ,( " array_string_test[2:]  ", 
        OArray (Array.fromList [OString "c__",OString "d__",OString "e__", OString "f__"])   )
--}

   ]

test_list4 =  [  -- dict

    ( " {\"ab\" : 1, \"xy\" : 2}   ", 
        ODict (Dict.fromList [("ab",OFloat 1),("xy",OFloat 2)])   )

   ,( " {}   ", 
        ODict (Dict.fromList [])   )

   ,( " {\"ab\" : \"1\", \"xy\" : \"2\"}   ", 
        ODict (Dict.fromList [("ab",OString "1"),("xy",OString "2")])   )

   ,( " {\"ab\" : [1,2,3], \"xy\" : [4,5,6]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])),
                              ("xy",OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 6]))
                             ])   )

   ,( " {\"ab\" : [test1,2,3], \"xy\" : [4,5,6+2]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OString "OKOK",OFloat 2,OFloat 3])),
                              ("xy",OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 8]))
                             ])   )

   ,( " {\"ab\" : [], \"xy\" : [4,5,6+2]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [])),
                              ("xy",OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 8]))
                             ])   )

   ,( " {\"ab\" : test1, \"xy\" : abc }   ", 
        ODict (Dict.fromList [("ab",OString "OKOK"),("xy",OString "_ABCD_")])   )

   ,( " {\"ab\" : test1 + \"_ok\", \"xy\" : abc }   ", 
        ODict (Dict.fromList [("ab",OString "OKOK_ok"),("xy",OString "_ABCD_")])   )

   ,( " {\"ab\" : test4, \"xy\" : abc }   ", 
        ODict (Dict.fromList [("ab",OString "err"),("xy",OString "_ABCD_")])   )


   ,( " {\"ab\" : [[1,2],[3,4]], \"xy\" : [[5,6],[7,8]]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OArray (Array.fromList [OFloat 1,OFloat 2])
                                                           ,OArray (Array.fromList [OFloat 3,OFloat 4])
                                                           ]
                                              )
                              )
                             ,("xy",OArray (Array.fromList [OArray (Array.fromList [OFloat 5,OFloat 6])
                                                           ,OArray (Array.fromList [OFloat 7,OFloat 8])
                                                           ]
                                              )  
                              )
                              ]) )

   ,( " {\"ab\" : [[],[3,4]], \"xy\" : [[5,6],[7,8]]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OArray (Array.fromList [])
                                                           ,OArray (Array.fromList [OFloat 3,OFloat 4])
                                                           ]
                                              )
                              )
                             ,("xy",OArray (Array.fromList [OArray (Array.fromList [OFloat 5,OFloat 6])
                                                           ,OArray (Array.fromList [OFloat 7,OFloat 8])
                                                           ]
                                              )  
                              )
                              ]) )

   ,( " {\"ab\" : [[1+1,2],[3,4]], \"xy\" : [[5,6],[7,8]]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OArray (Array.fromList [OFloat 2,OFloat 2])
                                                           ,OArray (Array.fromList [OFloat 3,OFloat 4])
                                                           ]
                                              )
                              )
                             ,("xy",OArray (Array.fromList [OArray (Array.fromList [OFloat 5,OFloat 6])
                                                           ,OArray (Array.fromList [OFloat 7,OFloat 8])
                                                           ]
                                              )  
                              )
                              ]) )

   ,( " {\"ab\" : array_test2d_1, \"xy\" : array_test2d_2 }   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OArray (Array.fromList [OFloat 1,OFloat 2])
                                                           ,OArray (Array.fromList [OFloat 3,OFloat 4])
                                                           ]
                                              )
                              )
                             ,("xy",OArray (Array.fromList [OArray (Array.fromList [OFloat 5,OFloat 6])
                                                           ,OArray (Array.fromList [OFloat 7,OFloat 8])
                                                           ]
                                              )  
                              )
                              ]) )


   ,( " {\"ab\" : [\"A\",\"B\",\"C\"], \"xy\" : [\"D\",\"E\",\"F\"]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OString "A",OString "B",OString "C"])),
                              ("xy",OArray (Array.fromList [OString "D",OString "E",OString "F"]))
                             ])   )

   ,( " {\"ab\" : [1,\"2\",3], \"xy\" : [4,\"5\",6]}   ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OFloat 1,OString "2",OFloat 3])),
                              ("xy",OArray (Array.fromList [OFloat 4,OString "5",OFloat 6]))
                             ])   )

   ,( " {\"ab\" : { \"a\" : 1, \"b\" : 2}, \"xy\" : { \"a\" : 3, \"b\" : 4}}   ", 
        ODict (Dict.fromList [("ab",ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2)]))
                             ,("xy",ODict (Dict.fromList [("a",OFloat 3),("b",OFloat 4)]))
                             ])   )

   ,( " {\"ab\" : { \"a\" : 1, \"b\" : 2}, \"xy\" : {  }}   ", 
        ODict (Dict.fromList [("ab",ODict (Dict.fromList [("a",OFloat 1),("b",OFloat 2)]))
                             ,("xy",ODict (Dict.fromList []))
                             ])   )

   ,( " dict_test.c   ", 
        OFloat 3   )

   ,( " dict_test{\"c\"}  ", 
        OFloat 3   )

   ,( " dict_test{dict_index}  ", 
        OFloat 3   )


   ,( " dict_test2  ", 
        ODict (Dict.fromList [("ab",OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3])),
                              ("xy",OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 6]))
                             ])   )
   ,( " dict_test2.xy   ", 
        OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 6])   )

   ,( " dict_test2{\"xy\"}   ", 
        OArray (Array.fromList [OFloat 4,OFloat 5,OFloat 6])   )


   ,( " dict_test2.xy[1]   ", 
        OFloat 5   )

   ,( " dict_test2{\"xy\"}[1]   ", 
        OFloat 5   )

   ,( " dict_test2.xy[index]   ", 
        OFloat 5   )

   ,( " dict_test2{dict_index2}[1 + index]   ", 
        OFloat 6   )
   ]

findstr1 = "A(BC)"
findstr2 = "A(\\d\\d\\d)"

test_list5 =  [  -- variable method
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

   ,( "  split_test.split(\",\")  ", 
        OArray (Array.fromList [OString "tom",OString "99",OString "90",OString "85"])   )
   ]

test_list6 =  [  -- typeof

  ( "  typeof(abc)         ",  OString "AvString" )

 ,( "  abc.type()          ",  OString "OString" )

 ,( "  abc.len()           ",  OFloat 6 )

 ,( "  array_test.len()    ",  OFloat 5 )

 ,( "  type(\"AB\")        ",  OString "OString" )

 ,( "  type(abc)           ",  OString "OString" )

 ,( "  type(array_test)    ",  OString "OArray" )

 ,( "  type(1)             ",  OString "OFloat" )
 ,( "  type(True)          ",  OString "OBool" )
 ,( "  type([1,2,3])       ",  OString "OArray" )
 ,( "  type({\"a\" : 1})   ",  OString "ODict" )
 ,( "  type([])            ",  OString "OArray" )
 ,( "  type({})            ",  OString "ODict" )

 ]

-----------------------------------------------------


r1 = test test_const test_enum test_func test_list1  
r2 = test test_const test_enum test_func test_list2  
r3 = test test_const test_enum test_func test_list3  
r4 = test test_const test_enum test_func test_list4  
r5 = test test_const test_enum test_func test_list5  
r6 = test test_const test_enum test_func test_list6  

e1 = test test_const test_enum test_func test_list1err  
e2 = test test_const test_enum test_func test_list2err  
e3 = test test_const test_enum test_func test_list3err  

c1 = test_contextdump  test_const test_enum test_func   

