

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

enum_get name_entry dic  =
    let
      args = Array.fromList (String.split ":" name_entry)
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
s3 = enum_get "City:Kobe" enumdic


enumdic2 = enum_reg_list enum_name1 enum_entry1_list Dict.empty
        |> enum_reg_list enum_name2 enum_entry2_list
        |> enum_reg_list enum_name3 enum_entry3_list

s4 = enum_get2 "City" "Kobe" enumdic2
s5 = enum_get "City:Kobe" enumdic2
