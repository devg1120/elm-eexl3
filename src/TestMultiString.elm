

module TestMultiString exposing (..)

import Parser exposing (..)

str = """abcd
efg
hijk
lmn
"""
str2 = "abcd/efg/hijk/lmn"

-------------------------------------------------------------------------
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

          --(r_2, c_2) =  case (String.uncons mstr) of
          --         Just ('\n', tstr_)  ->
          --                 (r_ + 1 , 1 )
          --         _ ->
          --                 (r_ , c_ + 1)

          mstr2 = String.dropLeft 1 mstr 
        in 
        if mstr2 == "" then
           "N/A over row"
        else
           getStringIndex mstr2 r c r_2 c_2

stringIndex str_ r c =
        getStringIndex str_ r c 1 1

r1 = stringIndex str 1 1 
r2 = stringIndex str 2 3 
r3 = stringIndex str 3 3  
