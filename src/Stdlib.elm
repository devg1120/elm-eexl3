module Stdlib exposing (..)

import Array
import Expr exposing (..)


strjoin : Context -> Input -> OutVal
strjoin context ar =
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
