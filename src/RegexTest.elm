module RegexTest exposing (..)

import Regex

------------------------------------ find
location : Regex.Regex
location =
  Maybe.withDefault Regex.never <|
    Regex.fromString "[oi]n a (\\w+)"

places : List Regex.Match
places =
  Regex.find location "I am on a boat in a lake."

--------------------------------------
loc : Regex.Regex
loc =
  Maybe.withDefault Regex.never <|
    Regex.fromString "A(\\d\\d\\d)"

loc2 : Regex.Regex
loc2 =
  Maybe.withDefault Regex.never <|
    Regex.fromString "A(BC)"

pla : List Regex.Match
pla =
  Regex.find loc2 "123 ABC aas qdd A987 SDDFGG A666"

--------------------------------------
r1 = places
r2 = pla

-------------------------------------- replace
userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
  case Regex.fromString userRegex of
    Nothing ->
      string

    Just regex ->
      Regex.replace regex replacer string

devowel : String -> String
devowel string =
  userReplace "[aeiou]" (\_ -> "_") string

r3 = devowel "The quick brown fox ao"

-------------------------------------- isContain

digit : Regex.Regex
digit =
  Maybe.withDefault Regex.never <|
    Regex.fromString "[0-9]"

r4 = Regex.contains digit "abc123" 
-------------------------------------- split
comma : Regex.Regex
comma =
  Maybe.withDefault Regex.never <|
    Regex.fromString ","

func a =
     String.replace " " "" a

r5 = Regex.split comma "tom , 99, 90, 85"

r6 = List.map func  (Regex.split comma "tom , 99, 90, 85")
