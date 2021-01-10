

module TestEnum exposing (..)

import Array
import Dict exposing (Dict)

{--
enum_reg name entry dic =
    let
         new_dic = Dict.insert name entry dic
    in
    case (Dict.get "_" new_dic) of
         Just a ->
                  Dict.insert "_" (Array.push name a )  new_dic 
         Nothing ->
                  Dict.insert "_" (Array.fromList [name])  new_dic

enum_get name entry dic =
    case (Dict.get "_" dic) of
         Just a ->
                  let
                     list = Array.toIndexedList a
                     func i n  =
                        let
                          index = Tuple.first i
                          value = Tuple.second i
                        in
                        if name == value then
                             index + 1
                        else
                             n
                   in
                   List.foldl func 0 list

         Nothing ->
                  -1
enum_count  dic =
    case (Dict.get "_" dic) of
         Just a ->
                  Array.length  a  
         Nothing ->
                  0

-----------------------------------------------
enumdic_empty = Dict.empty

enum_name1 = "Sushi"
enum_entry1 = Array.fromList ["Tako","Ika","Tai","Tamago","Maguro"]

enum_name2 = "Car"
enum_entry2 = Array.fromList ["Honda","Toyota","Suzuki","Nissan","Matsuda"]

enum_name3 = "City"
--enum_entry3 = Array.fromList ["Tokyo","Osaka","Kobe","Fukuoka","Nara"]
enum_entry3 = Array.fromList ["Tokyo","Osaka","Kobe","Fukuoka"]

r0 = enum_count enumdic_empty
enumdic = enum_reg enum_name1 enum_entry1 enumdic_empty
             |> enum_reg enum_name2 enum_entry2
             |> enum_reg enum_name3 enum_entry3

r1 = enum_count enumdic
r2 = enum_get "City" "Kobe" enumdic
r3 = enum_get "Sushi" "Kobe" enumdic
r4 = enum_get "No" "Kobe" enumdic
--}
-----------------------------------------------------
enum_reg name entry dic =
    let
        l = Array.length entry
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
              
        (entry_dic, len)  = Array.foldl f (entry_dic_empty, 0) entry

    in
    Dict.insert name entry_dic new_dic

enum_reg_list name entry dic =
    let
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

    in
    Dict.insert name entry_dic new_dic

enum_get2 name entry dic  =
    case (Dict.get name dic) of
         Just a ->
                  Dict.get  entry a  
         Nothing ->
                  Just (0,0)

enumNameToKey name_entry dic  =
    let
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


enumKeyToName key dic =
      let
         dic_list = dic_convert dic
         dict = Dict.fromList dic_list
      in
      Dict.get key dict

dic_convert dic =
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
           
dic_convert_ dic =
     let
        filter k v = 
             if k == "_" then
                 False
             else
                 True

        --dic_list = Dict.toList dic
        dic_list = Dict.toList (Dict.filter filter dic)
        fp e =
          let
            name = Tuple.first e
            edic = Tuple.second e
            edic_list = Dict.toList edic
            fe e2 =
               let
                 name2 = Tuple.first e2
                 edic2 = Tuple.second e2
               in
               (edic2,name ++ "::" ++ name2)
           in
           List.map fe edic_list
     in
     List.map fp dic_list

--enum_count2  dic =
--    case (Dict.get "_" dic) of
--         Just a ->
--                  Array.length  a  
--         Nothing ->
--                  0
--
-----------------------------------------------

enum_name1 = "Sushi"
enum_entry1 = Array.fromList ["Tako","Ika","Tai","Tamago","Maguro"]
enum_entry1_list =  ["Tako","Ika","Tai","Tamago","Maguro"]

enum_name2 = "Car"
enum_entry2 = Array.fromList ["Honda","Toyota","Suzuki","Nissan","Matsuda"]
enum_entry2_list =  ["Honda","Toyota","Suzuki","Nissan","Matsuda"]

enum_name3 = "City"
--enum_entry3 = Array.fromList ["Tokyo","Osaka","Kobe","Fukuoka","Nara"]
enum_entry3 = Array.fromList ["Tokyo","Osaka","Kobe","Fukuoka"]
enum_entry3_list =  ["Tokyo","Osaka","Kobe","Fukuoka"]

enumdic = enum_reg enum_name1 enum_entry1 Dict.empty
       |> enum_reg enum_name2 enum_entry2
       |> enum_reg enum_name3 enum_entry3

s2 = enum_get2 "City" "Kobe" enumdic
s3 = enumNameToKey "City::Kobe" enumdic


enumdic2 = enum_reg_list enum_name1 enum_entry1_list Dict.empty
        |> enum_reg_list enum_name2 enum_entry2_list
        |> enum_reg_list enum_name3 enum_entry3_list

s4 = enum_get2 "City" "Kobe" enumdic2
s5 = enumNameToKey "City::Kobe" enumdic2
d1 = dic_convert enumdic2
d2 = dic_convert_ enumdic2


l1 = enumKeyToName (3,3) enumdic2
l2 = enumKeyToName (1,4) enumdic2
