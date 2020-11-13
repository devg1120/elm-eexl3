module NegateTest exposing (..)

import Parser exposing (..)

myInt : Parser Int
myInt =
  oneOf
    [ succeed negate
        |. symbol "-"
        |= int
    , int
    ]

r1 = run int "123"       -- Ok
r2 = run int "-123"      -- Err
r3 = run myInt "123"     -- Ok 123
r4 = run myInt "-123"    -- Ok -123

-----------------------------------------
type Expr
    = Integer Int
    | Floating Float

digits : Parser Expr
digits =
    number
        { int = Just Integer
        , hex = Just Integer
        , octal = Nothing
        , binary = Nothing
        , float = Just Floating
        }


r5 =  run digits "1234"      -- Ok (Integer 1234)
r6 =  run digits "-123"      -- Ok (Integer -123)


