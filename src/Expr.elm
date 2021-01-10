--module Expr exposing (..)


module Expr exposing
    ( ArgValue(..)
    , Context(..)
    , Expr(..)
    , ExprResult(..)
    , Input
    , OutVal(..)
    , addConstant
    , addEnumDict
    , getEnumKey
    , getEnumString
    , addFunction
    , dicPop
    , dicPush
    , empty
    , evaluate
    , expression
    , formalArgValues
    , getConstant
    , getConstant2
    , newConstant
    ,  parse
       --    , exprexec  -- UNIT TEST
       --    , exprexec2 -- UNIT TEST

    , setConstant
    , typevar
    )

import Array
import Dict exposing (Dict)
import Parser exposing (..)
import Regex
import Set exposing (Set)
import Stack



-- EXPRESSIONS


type Expr
    = Integer Int
    | Floating Float
    | String String
    | Default Int
    | Enum String String
    | Array (Array.Array ArgValue)
    | Dict (Dict String ArgValue)
    | Bool Bool
    | Variable String
    | Function String (List Expr)
    | ArrayIndex String Expr
    --
    | ArrayIndexDictLookUp String Expr String
    | ArrayIndexDictIndex String Expr Expr
    --
    | Array2dIndex String Expr Expr
    | ArraySlice String ( Expr, Expr )
    | DictLookUp String String
    | DictLookUpArray String String Expr
    | DictIndex String Expr
    | DictIndexArray String Expr Expr
    | VariableMethod String String (List Expr)
    | Add Expr Expr --[+]  String Float
    | Sub Expr Expr --[-]  Float
    | Mul Expr Expr --[*]  Float
    | Div Expr Expr --[/]  Float
    | Div2 Expr Expr --[//] Float
    | Div3 Expr Expr --[%]  Float
    | And Expr Expr --[&&] Bool
    | Or Expr Expr --[||] Bool
    | LT Expr Expr --[<]  Bool
    | GT Expr Expr --[>]  Bool
    | LE Expr Expr --[<=] Bool
    | GE Expr Expr --[>=] Bool
    | EQ Expr Expr --[==] Bool
    | NE Expr Expr --[!=] Bool
    | Neg Expr --[-]  Float


type OutVal
    = OFloat Float
    | OString String
    | OBool Bool
    | OArray (Array.Array OutVal)
      --| ODict  (Dict OutVal OutVal)
    | ODict (Dict String OutVal)
    | OEnum (Int,Int)



--------------------------------------------------------- stack


dicGetSerch : List (Dict.Dict String OutVal) -> String -> Result String OutVal
dicGetSerch list name =
    let
        dict_ =
            List.head list |> Maybe.withDefault Dict.empty

        value =
            Dict.get name dict_
    in
    case value of
        Just a ->
            Ok a

        _ ->
            let
                new_list =
                    List.drop 1 list
            in
            if List.isEmpty new_list then
                --Err ("dicGetSerch...not found:" ++ name)
                Err ("NotFound1:" ++ name)

            else
                dicGetSerch new_list name


dicGet : String -> Stack.Stack (Dict.Dict String OutVal) -> Result String OutVal
dicGet name stackdic =
    let
        list =
            Stack.toList stackdic
    in
    dicGetSerch list name


dicSetUpdate : String -> OutVal -> Stack.Stack (Dict.Dict String OutVal) -> Result String (Stack.Stack (Dict.Dict String OutVal))
dicSetUpdate name value stackdic =
    let
        ( dict1, stack_ ) =
            Stack.pop stackdic

        dict_2 =
            case dict1 of
                Just dict_ ->
                    dict_

                _ ->
                    Dict.empty

        value2 =
            Dict.get name dict_2
    in
    case value2 of
        Just a ->
            let
                tmp_dict =
                    Dict.insert name value dict_2
            in
            Ok (Stack.push tmp_dict stack_)

        _ ->
            let
                tmp_list =
                    Stack.toList stack_
            in
            if List.isEmpty tmp_list then
                Err ("dicSetUpdate...not found:" ++ name)

            else
                let
                    new_stack_pair =
                        dicSetUpdate name value stack_
                in
                case new_stack_pair of
                    Ok new_stack_ ->
                        Ok (Stack.push dict_2 new_stack_)

                    Err str ->
                        Err str


dicSet : String -> OutVal -> Stack.Stack (Dict.Dict String OutVal) -> Result String (Stack.Stack (Dict.Dict String OutVal))
dicSet name value stackdic =
    let
        result =
            dicSetUpdate name value stackdic
    in
    result


dicSetNewLocal : String -> OutVal -> Stack.Stack (Dict.Dict String OutVal) -> Stack.Stack (Dict.Dict String OutVal)
dicSetNewLocal name value stackdic =
    let
        ( dict1, stack_ ) =
            Stack.pop stackdic

        dict_2 =
            case dict1 of
                Just dict_ ->
                    dict_

                _ ->
                    Dict.empty

        value2 =
            Dict.insert name value dict_2
    in
    Stack.push value2 stack_


dicPop : Stack.Stack (Dict.Dict String OutVal) -> Stack.Stack (Dict.Dict String OutVal)
dicPop stackdic =
    let
        ( a, newdic ) =
            Stack.pop stackdic
    in
    newdic


dicPush : Stack.Stack (Dict.Dict String OutVal) -> Stack.Stack (Dict.Dict String OutVal)
dicPush stackdic =
    let
        dict_ =
            Dict.empty
    in
    Stack.push dict_ stackdic


dicInit : Stack.Stack (Dict.Dict String OutVal)
dicInit =
    Stack.initialise



--------------------------------------------------------- context


type Context
    = Context
        { constants : Stack.Stack (Dict.Dict String OutVal)
        , functions : Dict String (Context -> Input -> OutVal)
        , enumdic   : Dict.Dict String (Dict.Dict String ( Int, Int ))
        , functop : Bool
        , return : Bool
        , break : Bool
        , continue : Bool
        , log : String
        , scope : Bool
        , defvar : Bool
        }


empty : Context
empty =
    Context
        { constants = dicInit
        , functions = Dict.empty
        , enumdic   = Dict.empty
        , functop = False
        , return = False
        , break = False
        , continue = False
        , log = ""
        , scope = True
        , defvar = True
        }


addEnumDict : String -> List String -> Context -> Context
addEnumDict name entry (Context context) =
    let
      dic = context.enumdic
      l = List.length entry
      (new_dic,num) = 
         case (Dict.get "_" dic) of
                   Just a ->
                            let
                              n_ = Dict.size a
                              n = n_ + 1
                            in
                            (Dict.insert "_" (Dict.insert name (n,l) a ) dic ,n)
                   Nothing ->
                            let
                             d = Dict.empty
                            in
                            (Dict.insert "_" (Dict.insert name (1,l) d)  dic ,1)

      entry_dic_empty = Dict.empty
      f name_ (dic_, n) =
            let
               n2 = n + 1
            in
            (Dict.insert name_ (num,n2) dic_, n2)
            
      (entry_dic, len)  = List.foldl f (entry_dic_empty, 0) entry

      new_dic_all = Dict.insert name entry_dic new_dic

    in
    Context
        { context
            | enumdic = new_dic_all
        }


getEnumKey : String -> Context -> Maybe ( Int, Int )
getEnumKey name_entry  (Context context) =
    let
      dic = context.enumdic
      args = Array.fromList (String.split "::" name_entry)
      name  = Array.get 0 args
      entry  = Array.get 1 args
    in
    case (name,entry) of
      (Just name_,Just entry_) ->
          case (Dict.get name_ dic) of
               Just a ->
                        Dict.get  entry_ a  
               Nothing ->
                        Just (0,0)
      _ ->
          Just (0,0)


enumdic_convert  : Dict.Dict String (Dict.Dict String b) -> List ( b, String )

enumdic_convert dic =
     let
        filter k v = 
             if k == "_" then
                 False
             else
                 True

        dic_list = Dict.toList (Dict.filter filter dic)
        fp e li =
          let
            name = Tuple.first e
            edic = Tuple.second e
            edic_list = Dict.toList edic
            fe e2 li2 =
               let
                 name2 = Tuple.first e2
                 edic2 = Tuple.second e2
                 ele = (edic2,name ++ "::" ++ name2) 
               in
               (::) ele  li2
           in
           List.foldl fe li edic_list 

     in
     List.foldl fp [] dic_list 

--getEnumString : comparable -> Context -> Maybe String
getEnumString : (Int, Int) -> Context -> Maybe String
getEnumString key  (Context context) =
    let
      dic = context.enumdic
      dic_list = enumdic_convert dic
      dict_ = Dict.fromList dic_list
    in
    Dict.get key dict_





addConstant : String -> OutVal -> Context -> Context
addConstant name value (Context context) =
    let
        r =
            dicSet name value context.constants

        ( new_constants, log_ ) =
            case r of
                Ok a ->
                    let
                        log =
                            context.log ++ ">" ++ name ++ " "
                    in
                    ( a, log )

                Err a ->
                    let
                        log =
                            context.log ++ "*" ++ name ++ " "
                    in
                    ( dicSetNewLocal name value context.constants, log )
    in
    Context
        { context
            | constants = new_constants
            , log = log_
        }


newConstant : String -> OutVal -> Context -> Context
newConstant name value (Context context) =
    let
        log =
            context.log ++ "*" ++ name ++ " "

        ( new_constants, log_ ) =
            ( dicSetNewLocal name value context.constants, log )
    in
    Context
        { context
            | constants = new_constants
            , log = log_
        }


setConstant : String -> OutVal -> Context -> Context
setConstant name value (Context context) =
    let
        r =
            dicSet name value context.constants

        ( new_constants, log_ ) =
            case r of
                Ok a ->
                    let
                        log =
                            context.log ++ ">" ++ name ++ " "
                    in
                    ( a, log )

                Err a ->
                    let
                        log =
                            context.log ++ "@" ++ name ++ " "
                    in
                    ( context.constants, log )
    in
    Context
        { context
            | constants = new_constants
            , log = log_
        }


addFunction : String -> (Context -> Input -> OutVal) -> Context -> Context
addFunction name f (Context context) =
    Context
        { context
            | functions = context.functions |> Dict.insert name f
        }


getConstant : String -> Context -> Maybe OutVal
getConstant name (Context { constants }) =
    let
        r =
            dicGet name constants
    in
    case r of
        Ok a ->
            Just a

        Err a ->
            Just (OString a)


getConstant2 : String -> Context -> Result String OutVal
getConstant2 name (Context { constants }) =
    dicGet name constants


getFunction : String -> Context -> Maybe (Context -> Input -> OutVal)
getFunction name (Context { functions }) =
    Dict.get name functions

-------------------------------------------------------- basic func
func_type : Array.Array OutVal -> OutVal
func_type args =
      case (Array.get 0 args) of
         Just (OString a) ->
              OString "OString"

         Just (OFloat a) ->
              OString "OFloat"

         Just (OBool a) ->
              OString "OBool"

         Just (OArray a) ->
              OString "OArray"

         Just (ODict a) ->
              OString "ODict"

         _ ->
              OString "unknown"

getBasicFunction : String ->  Maybe ( Array.Array OutVal -> OutVal)
getBasicFunction name  =
      case name of
          "type" ->
                  Just func_type 
          _ ->
                  Nothing

--------------------------------------------------------- evalate


type ExprResult error value notfound
    = ExprOk value
    | ExprNotFoundFunc notfound
    | ExprErr error


argsToAvArgs : userenv -> (userenv -> Context -> String -> Array.Array ArgValue -> OutVal) -> Context -> List Expr -> Input
argsToAvArgs userenv userfunc context exprs =
    let
        func3 expr =
            let
                value =
                    evaluate userenv userfunc context expr
            in
            case value of
                ExprOk (OString a) ->
                    AvString a

                ExprOk (OFloat a) ->
                    AvFloat a

                ExprOk (OBool a) ->
                    AvBool a

                _ ->
                    AvString "CVNG"

        args =
            List.map func3 exprs
    in
    Array.fromList args

argsToOutVal : userenv -> (userenv -> Context -> String -> Array.Array ArgValue -> OutVal) -> Context -> List Expr -> Array.Array OutVal
argsToOutVal userenv userfunc context exprs =
    let
        func3 expr =
            let
                value =
                    evaluate userenv userfunc context expr
            in
            case value of
                ExprOk  a ->
                     a

                _ ->
                    OString  "err"

        args =
            List.map func3 exprs
    in
    Array.fromList args

evaluate : userenv -> (userenv -> Context -> String -> Array.Array ArgValue -> OutVal) -> Context -> Expr -> ExprResult String OutVal ( String, Array.Array ArgValue )
evaluate userenv userfunc context expr =
    case expr of
        Variable name ->
            let
                value =
                    getConstant2 name context
            in
            case value of
                Ok v ->
                    ExprOk v

                Err v ->
                    ExprErr v

--        Function name args ->
--            let
--                func_ =
--                    getFunction name context
--
--                args_ =
--                    argsToAvArgs userenv userfunc context args
--            in
--            case func_ of
--                Just f ->
--                    ExprOk (f context args_)
--
--                _ ->
--                    --ExprNotFoundFunc  ("**not_found function:" ++ name)
--                    --ExprNotFoundFunc  (name,args)
--                    ExprOk (userfunc userenv context name args_)

        Function name args ->
            let
                func__ =
                    getBasicFunction name 

            in
            case func__ of
                Just f ->
                    --ExprOk (f context args)
                    let
                       args2 = 
                           argsToOutVal userenv userfunc context args
                    in
                    ExprOk (f  args2)

                _ ->
                    let
                        func_ =
                            getFunction name context

                        args_ =
                            argsToAvArgs userenv userfunc context args
                    in
                    case func_ of
                        Just f2 ->
                            ExprOk (f2 context args_)

                        _ ->
                            ExprOk (userfunc userenv context name args_)


        VariableMethod variable_name func_name args ->
            let
                value =
                    getConstant2 variable_name context

                args_ =
                    argsToAvArgs userenv userfunc context args
            in
            case value of
                Ok v ->
                    case func_name of
                        "type" ->
                            case v of
                                OString s ->
                                    ExprOk (OString "OString")

                                OArray a ->
                                    ExprOk (OString "OArray")

                                _ ->
                                    ExprErr "typeof() err"

                        "len" ->
                            case v of
                                OString s ->
                                    let
                                        len =
                                            toFloat (String.length s)
                                    in
                                    ExprOk (OFloat len)

                                OArray a ->
                                    let
                                        len =
                                            toFloat (Array.length a)
                                    in
                                    ExprOk (OFloat len)

                                _ ->
                                    ExprErr "len() err"

                        "sub" ->
                            let
                                a_ =
                                    Array.get 0 args_

                                b_ =
                                    Array.get 1 args_
                            in
                            case ( a_, b_, v ) of
                                ( Just (AvFloat a1), Just (AvFloat b2), OString s ) ->
                                    let
                                        str =
                                            String.slice (round a1) (round a1 + round b2) s
                                    in
                                    ExprOk (OString str)

                                ( Just (AvFloat a1), Just (AvFloat b2), OArray ar ) ->
                                    let
                                        ar_ =
                                            Array.slice (round a1) (round a1 + round b2) ar
                                    in
                                    ExprOk (OArray ar_)

                                _ ->
                                    ExprErr "sub() err expect String/Array"

                        "find" ->
                            let
                                a_ =
                                    Array.get 0 args_
                            in
                            case ( a_, v ) of
                                ( Just (AvString a1), OString s ) ->
                                    let
                                        regex =
                                            Maybe.withDefault Regex.never <|
                                                Regex.fromString a1

                                        result =
                                            Regex.find regex s

                                        func_5 a =
                                            let
                                                s_ =
                                                    a.submatches
                                            in
                                            s_

                                        list_ =
                                            List.map func_5 result

                                        func_2 a =
                                            List.head a

                                        list_2 =
                                            List.map func_2 list_

                                        arr_ =
                                            Array.fromList list_2

                                        func_3 a =
                                            case a of
                                                Just a3 ->
                                                    case a3 of
                                                        Just a4 ->
                                                            OString a4

                                                        _ ->
                                                            OString "find err1"

                                                _ ->
                                                    OString "find err2"

                                        arr_2 =
                                            Array.map func_3 arr_
                                    in
                                    ExprOk (OArray arr_2)

                                _ ->
                                    ExprErr "find() err"

                        "isContain" ->
                            let
                                a_ =
                                    Array.get 0 args_
                            in
                            case ( a_, v ) of
                                ( Just (AvString a1), OString s ) ->
                                    let
                                        regex =
                                            Maybe.withDefault Regex.never <|
                                                Regex.fromString a1

                                        result =
                                            Regex.contains regex s
                                    in
                                    case result of
                                        True ->
                                            ExprOk (OBool True)

                                        False ->
                                            ExprOk (OBool False)

                                _ ->
                                    ExprErr "isCotain() err"

                        "replace" ->
                            let
                                a_ =
                                    Array.get 0 args_

                                b_ =
                                    Array.get 1 args_
                            in
                            case ( a_, b_, v ) of
                                ( Just (AvString a1), Just (AvString b1), OString s ) ->
                                    let
                                        regex =
                                            Maybe.withDefault Regex.never <|
                                                Regex.fromString a1

                                        acf _ =
                                            b1

                                        result =
                                            Regex.replace regex acf s
                                    in
                                    ExprOk (OString result)

                                _ ->
                                    ExprErr "isCotain() err"

                        "split" ->
                            let
                                a_ =
                                    Array.get 0 args_
                            in
                            case ( a_, v ) of
                                ( Just (AvString a1), OString s ) ->
                                    let
                                        regex =
                                            Maybe.withDefault Regex.never <|
                                                Regex.fromString a1

                                        result =
                                            Regex.split regex s

                                        strip a =
                                            String.replace " " "" a

                                        result2 =
                                            List.map strip result

                                        arr_ =
                                            Array.fromList result2

                                        func_3 a =
                                            OString a

                                        arr_2 =
                                            Array.map func_3 arr_
                                    in
                                    ExprOk (OArray arr_2)

                                _ ->
                                    ExprErr "isCotain() err"

                        "match" ->
                            ExprOk (OBool True)

                        _ ->
                            ExprErr ("vriable method err:" ++ func_name)

                Err e ->
                    ExprErr ("not_found constant:" ++ variable_name)

        ArrayIndex name index_expr ->
            let
                array_ =
                    getConstant name context

                index_ =
                    evaluate userenv userfunc context index_expr
            in
            case array_ of
                Just a ->
                    case a of
                        OArray a_ ->
                            case index_ of
                                ExprOk (OFloat aa) ->
                                    let
                                        index =
                                            floor aa

                                        a2 =
                                            Array.get index a_
                                    in
                                    case a2 of
                                        Just a2_ ->
                                            ExprOk a2_

                                        _ ->
                                            ExprErr ("out of index:" ++ String.fromInt index)

                                ExprErr err ->
                                    ExprErr ("out of index:" ++ err)

                                _ ->
                                    ExprErr "out of index: must Int/Float"

                        _ ->
                            ExprErr "must Array/String"

                _ ->
                    ExprErr "must Array/String"

        ArrayIndexDictLookUp name index_expr dict_key ->
            let
                array_ =
                    getConstant name context

                index_ =
                    evaluate userenv userfunc context index_expr
            in
            case array_ of
                Just a ->
                    case a of
                        OArray a_ ->
                            case index_ of
                                ExprOk (OFloat aa) ->
                                    let
                                        index =
                                            floor aa

                                        a2 =
                                            Array.get index a_
                                    in
                                    case a2 of
                                        Just (ODict a2_) ->
                                            let
                                               d2 = Dict.get dict_key a2_ 
                                            in
                                            case d2 of
                                               Just a3_ ->
                                                   ExprOk a3_
                                               _ ->
                                                  ExprErr ("not found key:" ++ dict_key)

                                        _ ->
                                            ExprErr ("out of index:" ++ String.fromInt index)

                                ExprErr err ->
                                    ExprErr ("out of index:" ++ err)

                                _ ->
                                    ExprErr "out of index: must Int/Float"

                        _ ->
                            ExprErr "must Array/String"

                _ ->
                    ExprErr "must Array/String"

        ArrayIndexDictIndex name index_expr dict_key_expr ->
            let
                array_ =
                    getConstant name context

                index_ =
                    evaluate userenv userfunc context index_expr
                dict_key_ =
                    evaluate userenv userfunc context dict_key_expr
            in
            case array_ of
                Just a ->
                    case a of
                        OArray a_ ->
                            case (index_,dict_key_) of
                                (ExprOk (OFloat aa), ExprOk (OString key)) ->
                                    let
                                        index =
                                            floor aa

                                        a2 =
                                            Array.get index a_
                                    in
                                    case a2 of
                                        Just (ODict a2_) ->
                                            case (Dict.get key a2_ ) of
                                               Just a3_ ->
                                                   ExprOk a3_
                                               _ ->
                                                  ExprErr ("not found key:" ++ key)

                                        _ ->
                                            ExprErr ("out of index:" ++ String.fromInt index)

                                (ExprErr err,_) ->
                                    ExprErr ("out of index:" ++ err)

                                (_,ExprErr err) ->
                                    ExprErr ("out of index:" ++ err)

                                _ ->
                                    ExprErr "out of index: must Int/Float"

                        _ ->
                            ExprErr "must Array/String"

                _ ->
                    ExprErr "must Array/String"

        Array2dIndex name index_expr1 index_expr2 ->
            let
                array_ =
                    getConstant name context

                index_ =
                    evaluate userenv userfunc context index_expr1

                index2_ =
                    evaluate userenv userfunc context index_expr2

                get2d arr ind =
                                  case arr of
                                      OArray a_ ->
                                          case ind of
                                              ExprOk (OFloat aa) ->
                                                  let
                                                      index =
                                                          floor aa

                                                      a2 =
                                                          Array.get index a_
                                                  in
                                                  case a2 of
                                                      Just a2_ ->
                                                          ExprOk a2_

                                                      _ ->
                                                          ExprErr ("out of index:" ++ String.fromInt index)

                                              ExprErr err ->
                                                  ExprErr ("out of index:" ++ err)

                                              _ ->
                                                  ExprErr "out of index: must Int/Float"

                                      _ ->
                                          ExprErr "must Array/String"



            in
            case array_ of
                Just a ->
                    case a of
                        OArray a_ ->
                            case index_ of
                                ExprOk (OFloat aa) ->
                                    let
                                        index =
                                            floor aa

                                        a2 =
                                            Array.get index a_
                                    in
                                    case a2 of
                                        Just a2_ ->
                                            --ExprOk a2_
                                            get2d a2_ index2_

                                        _ ->
                                            ExprErr ("out of index:" ++ String.fromInt index)

                                ExprErr err ->
                                    ExprErr ("out of index:" ++ err)

                                _ ->
                                    ExprErr "out of index: must Int/Float"

                        _ ->
                            ExprErr "must Array/String"

                _ ->
                    ExprErr "must Array/String"

        ArraySlice name pair_expr ->
            let
                index1_expr =
                    Tuple.first pair_expr

                index2_expr =
                    Tuple.second pair_expr

                array_ =
                    getConstant name context

                index1_ =
                    case index1_expr of
                        Default a ->
                            ExprOk (OFloat -999)

                        _ ->
                            evaluate userenv userfunc context index1_expr


                index2_ =
                    case index2_expr of
                        Default a ->
                            ExprOk (OFloat -999)

                        _ ->
                            evaluate userenv userfunc context index2_expr


                in
                case array_ of
                   Just a ->
                       case a of
                           OArray a_ ->
                             case (index1_,index2_) of
                               ((ExprOk (OFloat ind1)),(ExprOk (OFloat ind2))) ->
                                   let
                                       index1 =  floor ind1
                                       index2 =  floor ind2
                                       i1 =
                                           if index1 == -999 then
                                               0

                                           else if index1 >= 0 then
                                               index1

                                           else
                                               Array.length a_ + index1

                                       i2 =
                                           if index2 == -999 then
                                               Array.length a_

                                           else if index2 >= 0 then
                                               index2

                                           else
                                               Array.length a_ + index2

                                       a2 =
                                           --Array.slice index1 index2 a_
                                           Array.slice i1 i2 a_
                                   in
                                   ExprOk (OArray a2)

                               ((ExprErr str), _) ->
                                   ExprErr str

                               (_ ,(ExprErr str)) ->
                                   ExprErr str

                               (_ , _) ->
                                   ExprErr  "not_found arrayIndex1"
                           _ ->
                               ExprErr  ("NotFound array:" ++ name)

                   _ ->
                       ExprErr  "not_found arrayIndex3"
            
           
        DictLookUp name key ->
            let
                dict_ =
                    getConstant name context

                ans =
                    case dict_ of
                        Just d ->
                            case d of
                                ODict d_ ->
                                    let
                                        d2 =
                                            Dict.get key d_
                                    in
                                    case d2 of
                                        Just d2_ ->
                                            d2_

                                        _ ->
                                            OString " !!not_found dictlookup"

                                _ ->
                                    OString " !!not_found dict lookuo"

                        _ ->
                            OString " !!not_found dict lookup"
            in
            ExprOk ans

        DictLookUpArray name key index_ ->
            let
                dict_ =
                    getConstant name context

                index2 = evaluate userenv userfunc context index_

                getArray arr ind =
                                  case arr of
                                      OArray a_ ->
                                          case ind of
                                              ExprOk (OFloat aa) ->
                                                  let
                                                      index =
                                                          floor aa

                                                      a2 =
                                                          Array.get index a_
                                                  in
                                                  case a2 of
                                                      Just a2_ ->
                                                          ExprOk a2_

                                                      _ ->
                                                          ExprErr ("out of index:" ++ String.fromInt index)

                                              ExprErr err ->
                                                  ExprErr ("out of index:" ++ err)

                                              _ ->
                                                  ExprErr "out of index: must Int/Float"

                                      _ ->
                                          ExprErr "must Array"
                in
                    case dict_ of
                        Just d ->
                            case d of
                                ODict d_ ->
                                    let
                                        d2 =
                                            Dict.get key d_
                                    in
                                    case d2 of
                                        Just d2_ ->
                                            --d2_
                                            getArray d2_ index2


                                        _ ->
                                            ExprErr " !!not_found dictlookup"

                                _ ->
                                    ExprErr " !!not_found dict lookup"

                        _ ->
                            ExprErr " !!not_found dict lookup"

        DictIndex name key_ ->
            let
                dict_ =
                    getConstant name context

                key2 = evaluate userenv userfunc context key_
                key = case key2 of
                              ExprOk (OString d) ->
                                        d
                              _ ->
                                        "N/A"

                ans =
                    case dict_ of
                        Just d ->
                            case d of
                                ODict d_ ->
                                    let
                                        d2 =
                                            Dict.get key d_
                                    in
                                    case d2 of
                                        Just d2_ ->
                                            d2_

                                        _ ->
                                            OString " !!not_found dictIndex"

                                _ ->
                                    OString " !!not_found dictIndex"

                        _ ->
                            OString " !!not_found dictIndex"
            in
            ExprOk ans

        DictIndexArray name key_ index_ ->
            let
                dict_ =
                    getConstant name context

                key2 = evaluate userenv userfunc context key_
                key = case key2 of
                              ExprOk (OString d) ->
                                        d
                              _ ->
                                        "N/A"
                index2 = evaluate userenv userfunc context index_

                getArray arr ind =
                                  case arr of
                                      OArray a_ ->
                                          case ind of
                                              ExprOk (OFloat aa) ->
                                                  let
                                                      index =
                                                          floor aa

                                                      a2 =
                                                          Array.get index a_
                                                  in
                                                  case a2 of
                                                      Just a2_ ->
                                                          ExprOk a2_

                                                      _ ->
                                                          ExprErr ("out of index:" ++ String.fromInt index)

                                              ExprErr err ->
                                                  ExprErr ("out of index:" ++ err)

                                              _ ->
                                                  ExprErr "out of index: must Int/Float"

                                      _ ->
                                          ExprErr "must Array"
                in
                    case dict_ of
                        Just d ->
                            case d of
                                ODict d_ ->
                                    let
                                        d2 =
                                            Dict.get key d_
                                    in
                                    case d2 of
                                        Just d2_ ->
                                            --d2_
                                            getArray d2_ index2

                                        _ ->
                                            ExprErr " !!not_found dictIndex"

                                _ ->
                                    ExprErr " !!not_found dictIndex"

                        _ ->
                            ExprErr " !!not_found dictIndex"

        String s ->
            ExprOk (OString s)

        Integer n ->
            ExprOk (OFloat (toFloat n))

        Floating n ->
            ExprOk (OFloat n)

        Bool n ->
            ExprOk (OBool n)

        Enum a b  ->
            let
              name = a ++ "::" ++ b
              key = getEnumKey name context
            in
            case key of
               Just aa ->
                   ExprOk (OEnum aa)
               _ ->
                   ExprErr ("enum not entry:"++ name)

        Array an ->
            let
                conv e =
                    case e of
                        AvInt n ->
                            OFloat (toFloat n)

                        AvBool n ->
                            OBool n

                        AvFloat n ->
                            OFloat n

                        AvString n ->
                            OString n

                        AvVar n ->
                            --OString n
                            --evaluate userenv userfunc context n
                            let
                                value =
                                    getConstant n context
                            in
                            case value of
                                    Just v_ ->
                                        v_
                                    _ ->
                                        OString (" AvVar not_found:" ++ n)

                        AvExpr n ->
                            --OString n
                            let
                               r = evaluate userenv userfunc context n
                            in
                            case r of
                               (ExprOk r_) ->
                                           r_
                               (_) ->
                                          OString  "err"

                        AvArray n ->                  -- Array2d
                            let
                               func_ a = 
                                  case a of
                                     AvString a_ ->
                                          OString a_
                                     AvInt a_ ->
                                          OFloat (toFloat a_)
                                     AvFloat a_ ->
                                          OFloat a_
                                     AvVar n_ ->
                                         let
                                             value =
                                                 getConstant n_ context
                                         in
                                         case value of
                                                 Just v_ ->
                                                     v_
                                                 _ ->
                                                     OString (" AvVar not_found:" ++ n_)

                                     AvExpr n_ ->
                                         let
                                            r = evaluate userenv userfunc context n_
                                         in
                                         case r of
                                            (ExprOk r_) ->
                                                        r_
                                            (_) ->
                                                       OString  "err"
                                     _ ->
                                         --OString "DEF"
                                         OString (Debug.toString a)
                            in
                            OArray (Array.map func_ n)

                        --AvDict n ->
                        --    let
                        --       func_ k2 v2 = 
                        --                OString v2

                        --    in
                        --    ODict (Dict.map func_ n)

                        AvDict n ->
                            let
                               func_ k2 v2 = 
                                     case v2 of
                                        AvString s ->
                                             OString s
                                        AvInt s ->
                                             OFloat (toFloat s)
                                        AvFloat s ->
                                             OFloat s

                                        AvArray s -> -- ADD
                                             let
                                                func_2 a = 
                                                   case a of
                                                      AvString a_ ->
                                                           OString a_
                                                      AvInt a_ ->
                                                           OFloat (toFloat a_)
                                                      AvFloat a_ ->
                                                           OFloat a_
                                                      AvVar n_ ->
                                                          let
                                                              value =
                                                                  getConstant n_ context
                                                          in
                                                          case value of
                                                                  Just v_ ->
                                                                      v_
                                                                  _ ->
                                                                      OString (" AvVar not_found:" ++ n_)

                                                      AvExpr n_ ->
                                                          --OString n
                                                          let
                                                             r = evaluate userenv userfunc context n_
                                                          in
                                                          case r of
                                                             (ExprOk r_) ->
                                                                         r_
                                                             (_) ->
                                                                         OString  "err"

                                                      _ ->
                                                           OString (Debug.toString a)
                                             in
                                             OArray (Array.map func_2 s)

                                        AvExpr s ->
                                             let
                                                r = evaluate userenv userfunc context s
                                             in
                                             case r of
                                                (ExprOk r_) ->
                                                            r_
                                                (_) ->
                                                           OString  "err"
                                        _ ->
                                             --OString "err"
                                             OString ("err1:" ++ Debug.toString v2)

                            in
                            ODict (Dict.map func_ n)

                arr2 =
                    Array.map conv an
            in
            ExprOk (OArray arr2)

        Dict dt ->
            let
                conv k v =
                    case v of
                        AvInt n ->
                            OFloat (toFloat n)

                        AvBool n ->
                            OBool n

                        AvFloat n ->
                            OFloat n

                        AvString n ->
                            OString n

                        AvVar n ->
                            --OString ("Var:"++ n)
                            --evaluate userenv userfunc context n
                            let
                                value =
                                    getConstant n context
                            in
                            case value of
                                    Just v_ ->
                                        v_
                                    _ ->
                                        OString (" AvVar not_found:" ++ n)


                        AvExpr n ->
                            --OString n
                            let
                               r = evaluate userenv userfunc context n
                            in
                            case r of
                               (ExprOk r_) ->
                                           r_
                               (_) ->
                                          OString  "err"

                        AvArray n ->
                            let
                               func_ a = 
                                  case a of
                                     AvString a_ ->
                                          OString a_
                                     AvInt a_ ->
                                          OFloat (toFloat a_)
                                     AvFloat a_ ->
                                          OFloat a_
                                     AvVar n_ ->
                                         let
                                             value =
                                                 getConstant n_ context
                                         in
                                         case value of
                                                 Just v_ ->
                                                     v_
                                                 _ ->
                                                     OString (" AvVar not_found:" ++ n_)

                                     AvExpr n_ ->
                                         let
                                            r = evaluate userenv userfunc context n_
                                         in
                                         case r of
                                            (ExprOk r_) ->
                                                        r_
                                            (_) ->
                                                       OString  "err"
--------------------------------------------------------
                                     AvArray n_2 ->
                                         let
                                            func_2 a2 = 
                                               case a2 of
                                                  AvString a_ ->
                                                       OString a_
                                                  AvInt a_ ->
                                                       OFloat (toFloat a_)
                                                  AvFloat a_ ->
                                                       OFloat a_
                                                  AvVar n_ ->
                                                      let
                                                          value =
                                                              getConstant n_ context
                                                      in
                                                      case value of
                                                              Just v_ ->
                                                                  v_
                                                              _ ->
                                                                  OString (" AvVar not_found:" ++ n_)

                                                  AvExpr n_ ->
                                                      let
                                                         r = evaluate userenv userfunc context n_
                                                      in
                                                      case r of
                                                         (ExprOk r_) ->
                                                                     r_
                                                         (_) ->
                                                                    OString  "err"

                                                  _ ->
                                                      --OString "DEF"
                                                      OString (Debug.toString a)
                                      in
                                      OArray (Array.map func_2 n_2)
--------------------------------------------------------
                                     _ ->
                                         --OString "DEF"
                                         OString (Debug.toString a)
                            in
                            OArray (Array.map func_ n)

                        --AvDict n ->
                        --    let
                        --       func_ k2 v2 = 
                        --                OString v2

                        --    in
                        --    ODict (Dict.map func_ n)

                        AvDict n ->
                            let
                               func_ k2 v2 = 
                                     case v2 of
                                        AvString s ->
                                             OString s
                                        AvInt s ->
                                             OFloat (toFloat s)
                                        AvFloat s ->
                                             OFloat s

                                        AvArray s -> -- ADD
                                             let
                                                func_2 a = 
                                                   case a of
                                                      AvString a_ ->
                                                           OString a_
                                                      AvInt a_ ->
                                                           OFloat (toFloat a_)
                                                      AvFloat a_ ->
                                                           OFloat a_
                                                      AvVar n_ ->
                                                          let
                                                              value =
                                                                  getConstant n_ context
                                                          in
                                                          case value of
                                                                  Just v_ ->
                                                                      v_
                                                                  _ ->
                                                                      OString (" AvVar not_found:" ++ n_)

                                                      AvExpr n_ ->
                                                          --OString n
                                                          let
                                                             r = evaluate userenv userfunc context n_
                                                          in
                                                          case r of
                                                             (ExprOk r_) ->
                                                                         r_
                                                             (_) ->
                                                                         OString  "err"

                                                      _ ->
                                                           OString (Debug.toString a)
                                             in
                                             OArray (Array.map func_2 s)

                                        AvExpr s ->
                                             let
                                                r = evaluate userenv userfunc context s
                                             in
                                             case r of
                                                (ExprOk r_) ->
                                                            r_
                                                (_) ->
                                                           OString  "err"
                                        _ ->
                                             OString ("err2:" ++ Debug.toString v2)

                            in
                            ODict (Dict.map func_ n)

                dt2 =
                    Dict.map conv dt
            in
            ExprOk (ODict dt2)

        Neg a ->
            let
                a_ =
                    evaluate userenv userfunc context a
            in
            case a_ of
                ExprOk (OFloat aa) ->
                    --ExprOk (OFloat (aa ))
                    ExprOk (OFloat (negate aa))

                _ ->
                    ExprErr "neg must Float"

        Add a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat aa), ExprOk (OFloat bb) ) ->
                    ExprOk (OFloat (aa + bb))

                ( ExprOk (OString aa), ExprOk (OString bb) ) ->
                    ExprOk (OString (aa ++ bb))

                ( ExprOk (OArray aa), ExprOk (OArray bb) ) ->
                    ExprOk (OArray (Array.append aa bb))

                ( ExprErr str, _ ) ->
                    ExprErr ("a + " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("+ b " ++ str)

                ( _, _ ) ->
                    ExprErr "+ value must Float/String/Array"

        Sub a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a__), ExprOk (OFloat b__) ) ->
                    ExprOk (OFloat (a__ - b__))

                ( ExprErr str, _ ) ->
                    ExprErr ("a - " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("- b " ++ str)

                ( _, _ ) ->
                    ExprErr "- value must Float"

        Mul a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a__), ExprOk (OFloat b__) ) ->
                    ExprOk (OFloat (a__ * b__))

                ( ExprErr str, _ ) ->
                    ExprErr ("a * " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("* b " ++ str)

                ( _, _ ) ->
                    ExprErr "* value must Float"

        Div a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a__), ExprOk (OFloat b__) ) ->
                    ExprOk (OFloat (a__ / b__))

                ( ExprErr str, _ ) ->
                    ExprErr ("a / " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("/ b " ++ str)

                ( _, _ ) ->
                    ExprErr "div value must Float"

        Div2 a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a__), ExprOk (OFloat b__) ) ->
                    ExprOk (OFloat (toFloat (floor a__ // floor b__)))

                ( ExprErr str, _ ) ->
                    ExprErr ("a // " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("// b " ++ str)

                ( _, _ ) ->
                    ExprErr "div2 value must Float"

        Div3 a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a__), ExprOk (OFloat b__) ) ->
                    let
                        a2 =
                            floor a__

                        b2 =
                            floor b__

                        div_ =
                            a2 // b2

                        ans_ =
                            a2 - (div_ * b2)
                    in
                    ExprOk (OFloat (toFloat ans_))

                ( ExprErr str, _ ) ->
                    ExprErr ("a % " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("% b " ++ str)

                ( _, _ ) ->
                    ExprErr "div3 value must Float"

        And a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OBool a__), ExprOk (OBool b__) ) ->
                    ExprOk (OBool (a__ && b__))

                ( ExprErr str, _ ) ->
                    ExprErr ("a && " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("&& b " ++ str)

                ( _, _ ) ->
                    ExprErr "&& value must Bool"

        Or a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OBool a__), ExprOk (OBool b__) ) ->
                    ExprOk (OBool (a__ || b__))

                ( ExprErr str, _ ) ->
                    ExprErr ("a || " ++ str)

                ( _, ExprErr str ) ->
                    ExprErr ("|| b " ++ str)

                ( _, _ ) ->
                    ExprErr "|| value must Bool"

        LT a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 < b1))

                _ ->
                    ExprErr "<  UnMatch Type Float"

        GT a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 > b1))

                _ ->
                    ExprErr ">  UnMatch Type Float"

        LE a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 <= b1))

                _ ->
                    ExprErr "<= UnMatch Type Float"

        GE a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 >= b1))

                _ ->
                    ExprErr ">= UnMatch Type Float"

        EQ a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 == b1))

                ( ExprOk (OString a1), ExprOk (OString b1) ) ->
                    ExprOk (OBool (a1 == b1))

                ( ExprOk (OBool a1), ExprOk (OBool b1) ) ->
                    ExprOk (OBool (a1 == b1))

                ( ExprOk (OEnum a1), ExprOk (OEnum b1) ) ->
                    ExprOk (OBool (a1 == b1))

                _ ->
                    ExprErr "== UnMatch Type Float/String/Bool"

        NE a b ->
            let
                a_ =
                    evaluate userenv userfunc context a

                b_ =
                    evaluate userenv userfunc context b
            in
            case ( a_, b_ ) of
                ( ExprOk (OFloat a1), ExprOk (OFloat b1) ) ->
                    ExprOk (OBool (a1 /= b1))

                ( ExprOk (OString a1), ExprOk (OString b1) ) ->
                    ExprOk (OBool (a1 /= b1))

                ( ExprOk (OBool a1), ExprOk (OBool b1) ) ->
                    ExprOk (OBool (a1 /= b1))

                ( ExprOk (OEnum a1), ExprOk (OEnum b1) ) ->
                    ExprOk (OBool (a1 /= b1))

                _ ->
                    ExprErr "!= UnMatch Type Float/String/Bool"

        Default a ->
            ExprOk (OBool True)


parse : String -> Result (List DeadEnd) Expr
parse string__ =
    run expression string__



-- STRINGS

string : Parser Expr
string =
    --succeed (String identity)
    succeed (\identity -> String identity)
        |. token "\""
        |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\n") (token "n")
                , map (\_ -> "\t") (token "t")
                , map (\_ -> "\u{000D}") (token "r")
                , succeed String.fromChar
                    |. token "u{"
                    |= unicode
                    |. token "}"
                ]
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
    getChompedString (chompWhile Char.isHexDigit)
        |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


typevar : Parser Expr
typevar =
    succeed (\identity -> Variable identity)
        |= typevarHelp


typevarHelp : Parser String
typevarHelp =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
        }


default : Parser Expr
default =
    succeed (Default 1)
        |. keyword "default"



---------------------------------------------

array_empty : Parser Expr
array_empty =
    succeed Tuple.pair
        |= backtrackable (symbol "[")
        |. spaces
        |= symbol "]"
        |> andThen
            (\( a, b ) ->
                let
                    base =
                        Array.fromList []
                in
                succeed (Array base)
            )

array : Parser Expr
array =
    succeed Tuple.pair
        |. backtrackable (symbol "[")
        |= arrayValues
        |= symbol "]"
        |> andThen
            (\( arg, a ) ->
                let
                    base =
                        Array.fromList arg
                in
                succeed (Array base)
            )

arrayValues : Parser (List ArgValue)
arrayValues =
    succeed (::)
        |. spaces
        |= oneOf
            [ backtrackable exprValue
            , backtrackable stringValue
            , backtrackable intValue
            , backtrackable floatValue
            , backtrackable dict2_empty
            , backtrackable dict2
            , backtrackable array2d_empty
            , backtrackable array2d
            --, backtrackable dict2
            , varValue
            --, backtrackable varValue
            --, exprValue
            ]
        |. spaces
        |= arrayValuesTail
        |> andThen
            (\arg ->
                succeed arg
            )


arrayValuesTail : Parser (List ArgValue)
arrayValuesTail =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= oneOf
                [ backtrackable exprValue
                , backtrackable stringValue
                , backtrackable intValue
                , backtrackable floatValue
                , backtrackable dict2_empty
                , backtrackable dict2
                , backtrackable array2d_empty
                , backtrackable array2d
                --, backtrackable dict2
                , varValue
                --, backtrackable varValue
                --, exprValue
                ]
            |. spaces
            |= lazy (\_ -> arrayValuesTail)
        , succeed []
        ]

array2d_empty : Parser ArgValue
array2d_empty =
    succeed Tuple.pair
        |= backtrackable (symbol "[")
        |. spaces
        |= symbol "]"
        |> andThen
            (\( a, b ) ->
                let
                    base =
                        Array.fromList []
                in
                succeed (AvArray base)
            )

array2d : Parser ArgValue
array2d =
    succeed Tuple.pair
        |. backtrackable (symbol "[")
        |= array2dValues
        |= symbol "]"
        |> andThen
            (\( arg, a ) ->
                let
                    base =
                        Array.fromList arg
                in
                succeed (AvArray base)
            )


array2dValues : Parser (List ArgValue)
array2dValues =
    succeed (::)
        |. spaces
        |= oneOf
            [ backtrackable exprValue
            , backtrackable stringValue
            , backtrackable intValue
            , backtrackable floatValue
            , varValue
            ]
        |. spaces
        |= array2dValuesTail
        |> andThen
            (\arg ->
                succeed arg
            )


array2dValuesTail : Parser (List ArgValue)
array2dValuesTail =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= oneOf
                [ backtrackable exprValue
                , backtrackable stringValue
                , backtrackable intValue
                , backtrackable floatValue
                , varValue
                ]
            |. spaces
            |= lazy (\_ -> array2dValuesTail)
        , succeed []
        ]

----------------------------------------------
array2_empty : Parser ArgValue
array2_empty =
    succeed Tuple.pair
        |= backtrackable (symbol "[")
        |= symbol "]"
        |> andThen
            (\( a, b ) ->
                let
                    base =
                        Array.fromList []
                in
                succeed (AvArray base)
            )

array2 : Parser ArgValue
array2 =
    succeed Tuple.pair
        |. backtrackable (symbol "[")
        |= arrayValues2
        |= symbol "]"
        |> andThen
            (\( arg, a ) ->
                let
                    base =
                        Array.fromList arg
                in
                succeed (AvArray base)
            )

arrayValues2 : Parser (List ArgValue)
arrayValues2 =
    succeed (::)
        |. spaces
        |= oneOf
            [ backtrackable exprValue
            , backtrackable stringValue
            , backtrackable intValue
            , backtrackable floatValue
            , backtrackable array2d_empty
            , backtrackable array2d
            , varValue
            ]
        |. spaces
        |= arrayValuesTail2
        |> andThen
            (\arg ->
                succeed arg
            )


arrayValuesTail2 : Parser (List ArgValue)
arrayValuesTail2 =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= oneOf
                [ backtrackable exprValue
                , backtrackable stringValue
                , backtrackable intValue
                , backtrackable floatValue
                , backtrackable array2d_empty
                , backtrackable array2d
                , varValue
                ]
            |. spaces
            |= lazy (\_ -> arrayValuesTail2)
        , succeed []
        ]



---------------------------------------------

dict_empty : Parser Expr
dict_empty =
    succeed Tuple.pair
        |= backtrackable (symbol "{")
        |. spaces
        |= symbol "}"
        |> andThen
            (\( a, b ) ->
                let
                    base =
                        Dict.fromList []
                in
                succeed (Dict base)
            )

dict : Parser Expr
dict =
    succeed Tuple.pair
        |. backtrackable (symbol "{")
        |= dictValues
        |= symbol "}"
        |> andThen
            (\( arg, a ) ->
                let
                    base =
                        Dict.fromList arg
                in
                succeed (Dict base)
            )


dictKey : Parser String
dictKey =
    succeed Just
        |. spaces
        |. symbol "\""
        |= getChompedString (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. spaces
        |> andThen
            (\arg ->
                --succeed (AvString (arg   |> Maybe.withDefault "" ))
                succeed (arg |> Maybe.withDefault "")
            )


dictKV : Parser ( String, ArgValue )
dictKV =
    succeed (\k v -> ( k, v ))
        |. spaces
        |= dictKey
        |. symbol ":"
        |. spaces
        |= oneOf
            [ backtrackable exprValue
            , backtrackable stringValue
            , backtrackable intValue
            , backtrackable floatValue
            , backtrackable arrayValue
            , backtrackable dict2_empty
            , backtrackable dict2
            , varValue
            ]


dictValues : Parser (List ( String, ArgValue ))
dictValues =
    succeed (::)
        |. spaces
        |= dictKV
        |. spaces
        |= dictValuesTail
        |> andThen
            (\arg ->
                succeed arg
            )


dictValuesTail : Parser (List ( String, ArgValue ))
dictValuesTail =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= dictKV
            |. spaces
            |= lazy (\_ -> dictValuesTail)
        , succeed []
        ]

---------------------------------------------

dict2_empty : Parser ArgValue
dict2_empty =
    succeed Tuple.pair
        |= backtrackable (symbol "{")
        |. spaces
        |= symbol "}"
        |> andThen
            (\( a, b ) ->
                let
                    base =
                        Dict.fromList []
                in
                succeed (AvDict base)
            )

dict2 : Parser ArgValue
dict2 =
    succeed Tuple.pair
        |. backtrackable (symbol "{")
        |= dictValues2
        |= symbol "}"
        |> andThen
            (\( arg, a ) ->
                let
                    base =
                        Dict.fromList arg
                in
                succeed (AvDict base)
            )


dictKey2 : Parser String
dictKey2 =
    succeed Just
        |. spaces
        |. symbol "\""
        |= getChompedString (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. spaces
        |> andThen
            (\arg ->
                --succeed (AvString (arg   |> Maybe.withDefault "" ))
                succeed (arg |> Maybe.withDefault "")
            )


dictKV2 : Parser ( String, ArgValue )
dictKV2 =
    succeed (\k v -> ( k, v ))
        |. spaces
        |= dictKey2
        |. symbol ":"
        |. spaces
        |= oneOf
            [ backtrackable exprValue
            , backtrackable stringValue
            , backtrackable intValue
            , backtrackable floatValue
            , backtrackable arrayValue
            , varValue
            ]


dictValues2 : Parser (List ( String, ArgValue ))
dictValues2 =
    succeed (::)
        |. spaces
        |= dictKV2
        |. spaces
        |= dictValuesTail2
        |> andThen
            (\arg ->
                succeed arg
            )


dictValuesTail2 : Parser (List ( String, ArgValue ))
dictValuesTail2 =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= dictKV2
            |. spaces
            |= lazy (\_ -> dictValuesTail2)
        , succeed []
        ]

----------------------------------------------


----------------------------------------------------------
-- FUNC def  start
----------------------------------------------------------


type ArgValue 
    = AvInt Int
    | AvBool Bool
    | AvFloat Float
    | AvString String
    | AvVar String
    | AvExpr Expr --
    | AvArray (Array.Array ArgValue)
    --| AvDict (Dict String String)
    | AvDict (Dict String ArgValue)


type alias Input =
    Array.Array ArgValue



-------------------------------------------------------------


stringValue : Parser ArgValue
stringValue =
    succeed Just
        |. spaces
        |. symbol "\""
        |= getChompedString (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. spaces
        |> andThen
            (\arg ->
                succeed (AvString (arg |> Maybe.withDefault ""))
            )



intValue : Parser ArgValue
intValue =
    succeed Just
        |. spaces
        |= int
        |. spaces
        |> andThen
            (\arg ->
                succeed (AvInt (arg |> Maybe.withDefault 0))
            )


floatValue : Parser ArgValue
floatValue =
    succeed Just
        |. spaces
        |= float
        |. spaces
        |> andThen
            (\arg ->
                succeed (AvFloat (arg |> Maybe.withDefault 0.0))
            )

--exprValue : Parser ArgValue
--exprValue =
--    succeed Just
--        |. spaces
--        |= expression5
--        |. spaces
--        |> andThen
--            (\arg ->
--                succeed (AvExpr (arg |> Maybe.withDefault 0.0))
--            )

exprValue : Parser ArgValue
exprValue =
    succeed AvExpr
        |. spaces
        |= expression5
        |. spaces

arrayValue : Parser ArgValue
arrayValue =
    succeed Just
        |. spaces
        --|= array2
        |= oneOf
           [ backtrackable array2_empty
           , array2
           ]
        |. spaces
        |> andThen
            (\arg ->
                succeed (arg |> Maybe.withDefault (AvString "N/A"))
            )


varValue : Parser ArgValue
varValue =
    succeed (\identity -> AvVar identity)
        |= varValueHelp


varValueHelp : Parser String
varValueHelp =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
        }



---------------------------------------------


argValues : Parser (List Expr)
argValues =
    succeed (::)
        |. spaces
        |= expression2
        |. spaces
        |= argValuesTail
        |> andThen
            (\arg ->
                succeed arg
            )


argValuesTail : Parser (List Expr)
argValuesTail =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= expression2
            |. spaces
            |= lazy (\_ -> argValuesTail)
        , succeed []
        ]



--------------------------------------------- formalArg


formalVarValue : Parser Expr
formalVarValue =
    succeed (\identity -> Variable identity)
        |= formalVarValueHelp


formalVarValueHelp : Parser String
formalVarValueHelp =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
        }


formalArgValues : Parser (List Expr)
formalArgValues =
    succeed (::)
        |. spaces
        |= formalVarValue
        |. spaces
        |= formalArgValuesTail
        |> andThen
            (\arg ->
                succeed arg
            )


formalArgValuesTail : Parser (List Expr)
formalArgValuesTail =
    oneOf
        [ succeed (::)
            |. symbol ","
            |. spaces
            |= formalVarValue
            |. spaces
            |= lazy (\_ -> formalArgValuesTail)
        , succeed []
        ]



-------------------------------------------------------------


func : Parser Expr
func =
    succeed Tuple.pair
        |= backtrackable
            (variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                }
            )
        |. backtrackable (symbol "(")
        |= argValues
        |. symbol ")"
        |> andThen
            (\( name, args ) ->
                succeed (Function name args)
            )



----------------------------------------------------------
-- FUNC def   end
----------------------------------------------------------


array_index : Parser Expr
array_index =
    succeed Tuple.pair
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= expression3
        |. symbol "]"
        |> andThen
            (\( name, index ) ->
                succeed (ArrayIndex name index)
            )

array_index_dict_lookup : Parser Expr
array_index_dict_lookup =
    succeed ArrayIndexDictLookUp
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= expression3
        |. symbol "]"
        |. symbol "."
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }

array_index_dict_index : Parser Expr
array_index_dict_index =
    succeed ArrayIndexDictIndex
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= expression3
        |. symbol "]"
        |. symbol "{"
        |= expression4
        |. symbol "}"


array2d_index : Parser Expr
array2d_index =
    --succeed Tuple.pair
    --succeed (String,(Expr,Expr))
    succeed Array2dIndex
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= expression3
        |. symbol "]"
        |. symbol "["
        |= expression3
        |. symbol "]"
      --  |> andThen
      --      (\ name index1 index2  ->
      --          succeed (Array2dIndex name index1 index2)
      --      )
            ----(\( name, index1, index2 ) ->
            --(\  args ->
            --    let
            --        arr_ = Array.fromList args
            --        name   = Array.get 2 arr_
            --        index1 = Array.get 1 arr_
            --        index2 = Array.get 0 arr_
            --    in
            --    succeed (Array2dIndex name index1 index2)
            -- )


slice : Parser ( Expr, Expr )
slice =
    succeed Tuple.pair
        |= expression3
        |. symbol ":"
        |= expression3
        |> andThen
            (\( index1, index2 ) ->
                succeed (Tuple.pair index1 index2)
            )




--  $$:
sliceleft : Parser ( Expr, Expr )
sliceleft =
    succeed (\index -> ( index, Default 0 ))
        |= expression3
        |. symbol ":"


--  :$$
sliceright : Parser ( Expr, Expr )
sliceright =
    succeed (\index -> ( Default 0, index ))
        |. symbol ":"
        |= expression3



array_slice : Parser Expr
array_slice =
    succeed Tuple.pair
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= oneOf
            [ backtrackable slice
            , backtrackable sliceleft
            , sliceright
            ]
        |> andThen
            (\( name, index_pair ) ->
                --succeed (ArrayIndex name index)
                succeed (ArraySlice name index_pair)
            )


dict_lookup : Parser Expr
dict_lookup =
    succeed Tuple.pair
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |. symbol "."
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |> andThen
            (\( name, key ) ->
                succeed (DictLookUp name key)
            )

dict_lookup_array : Parser Expr
dict_lookup_array =
    succeed DictLookUpArray
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |. symbol "."
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "["
        |= expression3
        |. symbol "]"


dict_index : Parser Expr
dict_index =
    succeed Tuple.pair
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "{"
        --|= string
        |= expression4
        |. symbol "}"
        |> andThen
            (\( name, key ) ->
                succeed (DictIndex name key)
            )

dict_index_array : Parser Expr
dict_index_array =
    succeed DictIndexArray
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "let", "fn", "return", "break", "continue" ]
            }
        |. symbol "{"
        --|= string
        |= expression4
        |. symbol "}"
        |. symbol "["
        |= expression3
        |. symbol "]"

variable_method : Parser Expr
variable_method =
    succeed VariableMethod
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |. symbol "."
        |= variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. symbol "("
        |= oneOf
            [ backtrackable argValues
            , succeed []
            ]
        |. symbol ")"


variable_method2 : Parser Expr
variable_method2 =
    succeed VariableMethod
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |. symbol "."
        |= variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. symbol "("
        |= argValues
        |. symbol ")"


enum : Parser Expr
enum =
    succeed Tuple.pair
        |= variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |. symbol ":"
        |. symbol ":"
        |= variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "if", "then", "else", "elsif", "while", "do", "end", "for", "case", "var", "def", "return", "break", "continue" ]
            }
        |> andThen
            (\( name, entry ) ->
                --succeed (DictIndex name entry)
                succeed (Enum name entry)
            )

-- PARSER


{-| We want to handle integers, hexadecimal numbers, and floats. Octal numbers
like `0o17` and binary numbers like `0b01101100` are not allowed.

    run digits "1234"      == Ok (Integer 1234)
    run digits "-123"      == Ok (Integer -123)
    run digits "0x1b"      == Ok (Integer 27)
    run digits "3.1415"    == Ok (Floating 3.1415)
    run digits "0.1234"    == Ok (Floating 0.1234)
    run digits ".1234"     == Ok (Floating 0.1234)
    run digits "1e-42"     == Ok (Floating 1e-42)
    run digits "6.022e23"  == Ok (Floating 6.022e23)
    run digits "6.022E23"  == Ok (Floating 6.022e23)
    run digits "6.022e+23" == Ok (Floating 6.022e23)
    run digits "6.022e"    == Err ..
    run digits "6.022n"    == Err ..
    run digits "6.022.31"  == Err ..

-}

digits : Parser Expr
digits =
    number
        { int = Just Integer
        , hex = Just Integer
        , octal = Nothing
        , binary = Nothing
        , float = Just Floating
        }


negdigits : Parser Expr
negdigits =
    succeed Neg
        |. symbol "-"
        |= digits


bool : Parser Expr
bool =
    let
        true =
            succeed (always (Bool True))
                |= keyword "True"
                |. spaces

        false =
            succeed (always (Bool False))
                |= keyword "False"
                |. spaces
    in
    oneOf [ true, false ]


term : Parser Expr
term =
    succeed (\a -> a)
        |. spaces
        |= oneOf
            --[ digits
            [ backtrackable string
            , backtrackable enum
            , backtrackable array_empty
            , backtrackable array
            , backtrackable dict_empty
            , backtrackable dict
            , backtrackable func
            , backtrackable array_slice
            , backtrackable array2d_index
            , backtrackable array_index_dict_lookup
            , backtrackable array_index_dict_index
            , backtrackable array_index
            , backtrackable variable_method
            , backtrackable dict_lookup_array
            , backtrackable dict_lookup
            , backtrackable dict_index_array
            , backtrackable dict_index
            , backtrackable default
            , backtrackable typevar
            , backtrackable negdigits
            , backtrackable digits
            , bool
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ")"
            ]
        |. spaces


expression : Parser Expr
expression =
    term
        |> andThen (expressionHelp [])


expressionHelp : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]



-----------------------------------------------------------


term2 : Parser Expr
term2 =
    succeed (\a -> a)
        |. spaces
        |= oneOf
            --[ digits
            [ backtrackable string
            , backtrackable enum
            , backtrackable array_empty
            , backtrackable array
            , backtrackable dict_empty
            , backtrackable dict

            --, backtrackable func
            , backtrackable array2d_index
            , backtrackable array_index
            , backtrackable dict_lookup
            , backtrackable dict_index
            , backtrackable default
            , backtrackable typevar
            , backtrackable digits
            , bool
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression2)
                |. spaces
                |. symbol ")"
            ]
        |. spaces


expression2 : Parser Expr
expression2 =
    term2
        |> andThen (expressionHelp2 [])


expressionHelp2 : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp2 revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term2
            |> andThen (\( op, newExpr ) -> expressionHelp2 (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]



-----------------------------------------------------------


term3 : Parser Expr
term3 =
    succeed (\a -> a)
        |. spaces
        |= oneOf
            --[ digits
            [ backtrackable string
            , backtrackable enum
            , backtrackable array
            , backtrackable dict

            --, backtrackable func
            --, backtrackable array_index
            , backtrackable dict_lookup
            , backtrackable dict_index
            , backtrackable default
            , backtrackable typevar
            , backtrackable negdigits
            , backtrackable digits
            , bool
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression2)
                |. spaces
                |. symbol ")"
            ]
        |. spaces


expression3 : Parser Expr
expression3 =
    term3
        |> andThen (expressionHelp3 [])


expressionHelp3 : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp3 revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term2
            |> andThen (\( op, newExpr ) -> expressionHelp3 (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]



-----------------------------------------------------------


term4 : Parser Expr
term4 =
    succeed (\a -> a)
        |. spaces
        |= oneOf
            --[ digits
            [ backtrackable string
            , backtrackable enum
            , backtrackable array
            , backtrackable dict

            --, backtrackable func
            --, backtrackable array_index
            --, backtrackable dict_lookup
            --, backtrackable dict_index
            , backtrackable default
            , backtrackable typevar
            , backtrackable negdigits
            , backtrackable digits
            , bool
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression2)
                |. spaces
                |. symbol ")"
            ]
        |. spaces


expression4 : Parser Expr
expression4 =
    term4
        |> andThen (expressionHelp4 [])


expressionHelp4 : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp4 revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term4
            |> andThen (\( op, newExpr ) -> expressionHelp4 (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


-----------------------------------------------------------


term5 : Parser Expr
term5 =
    succeed (\a -> a)
        |. spaces
        |= oneOf
            --[ digits
            [ backtrackable string
            , backtrackable enum
            --, backtrackable array
            --, backtrackable dict

            --, backtrackable func
            --, backtrackable array_index
            --, backtrackable dict_lookup
            --, backtrackable dict_index
            --, backtrackable default
            , backtrackable typevar
            , backtrackable negdigits
            , backtrackable digits
            , bool
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression5)
                |. spaces
                |. symbol ")"
            ]
        |. spaces


expression5 : Parser Expr
expression5 =
    term5
        |> andThen (expressionHelp5 [])


expressionHelp5 : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp5 revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term4
            |> andThen (\( op, newExpr ) -> expressionHelp5 (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]

-----------------------------------------------------------


type Operator
    = AddOp
    | SubOp
    | MulOp
    | DivOp
    | Div2Op
    | Div3Op
    | AndOp
    | OrOp
    | LTOp
    | GTOp
    | LEOp
    | GEOp
    | EQOp
    | NEOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> SubOp) (symbol "-")
        , map (\_ -> MulOp) (symbol "*")

        --, map (\_ -> DivOp) ( backtrackable (symbol "//"))
        --, map (\_ -> DivOp) ( backtrackable (symbol "/"))
        , map (\_ -> Div2Op) (symbol "//")
        , map (\_ -> DivOp) (symbol "/")
        , map (\_ -> Div3Op) (symbol "%")
        , map (\_ -> AndOp) (symbol "&&")
        , map (\_ -> OrOp) (symbol "||")

        --, map (\_ -> LTOp)  (symbol "<")
        --, map (\_ -> GTOp)  (symbol ">")
        , map (\_ -> LEOp) (symbol "<=")
        , map (\_ -> GEOp) (symbol ">=")
        , map (\_ -> LTOp) (symbol "<")
        , map (\_ -> GTOp) (symbol ">")
        , map (\_ -> EQOp) (symbol "==")
        , map (\_ -> NEOp) (symbol "!=")
        ]


finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, MulOp ) :: otherRevOps ->
            finalize otherRevOps (Mul expr finalExpr)

        ( expr, DivOp ) :: otherRevOps ->
            finalize otherRevOps (Div expr finalExpr)

        ( expr, Div2Op ) :: otherRevOps ->
            finalize otherRevOps (Div2 expr finalExpr)

        ( expr, Div3Op ) :: otherRevOps ->
            finalize otherRevOps (Div3 expr finalExpr)

        ( expr, AddOp ) :: otherRevOps ->
            Add (finalize otherRevOps expr) finalExpr

        ( expr, SubOp ) :: otherRevOps ->
            Sub (finalize otherRevOps expr) finalExpr

        ( expr, AndOp ) :: otherRevOps ->
            And (finalize otherRevOps expr) finalExpr

        ( expr, OrOp ) :: otherRevOps ->
            Or (finalize otherRevOps expr) finalExpr

        ( expr, LTOp ) :: otherRevOps ->
            LT (finalize otherRevOps expr) finalExpr

        ( expr, GTOp ) :: otherRevOps ->
            GT (finalize otherRevOps expr) finalExpr

        ( expr, LEOp ) :: otherRevOps ->
            LE (finalize otherRevOps expr) finalExpr

        ( expr, GEOp ) :: otherRevOps ->
            GE (finalize otherRevOps expr) finalExpr

        ( expr, EQOp ) :: otherRevOps ->
            EQ (finalize otherRevOps expr) finalExpr

        ( expr, NEOp ) :: otherRevOps ->
            NE (finalize otherRevOps expr) finalExpr



---------------------------------------------------------------------
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

--}
