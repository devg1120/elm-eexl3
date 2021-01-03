

module TestEnum exposing (..)

import Array
import Dict exposing (Dict)


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

enum_name = "Sushi"
enum_entry = Array.fromList ["Tako","Ika","Tai","Tamago","Maguro"]

enum_name2 = "Car"
enum_entry2 = Array.fromList ["Honda","Toyota","Suzuki","Nissan","Matsuda"]

enum_name3 = "City"
enum_entry3 = Array.fromList ["Tokyo","Osaka","Kobe","Fukuoka","Nara"]

r0 = enum_count enumdic_empty
enumdic = enum_reg enum_name enum_entry enumdic_empty
             |> enum_reg enum_name2 enum_entry2
             |> enum_reg enum_name3 enum_entry3

r1 = enum_count enumdic
r2 = enum_get "City" "Kobe" enumdic
r3 = enum_get "Sushi" "Kobe" enumdic
r4 = enum_get "No" "Kobe" enumdic




