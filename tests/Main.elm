module Main exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Piano.Note exposing (..)


suite : Test
suite =
    describe "naturalKeyDistance"
        [ test "C4 C3" <|
            \_ ->
                naturalKeyDistance 48 36
                    |> Expect.equal 7
        , test "C4 G4" <|
            \_ ->
                naturalKeyDistance 48 55
                    |> Expect.equal (-4)
        , test "C4 C4" <|
            \_ ->
                naturalKeyDistance 48 48
                    |> Expect.equal 0
        ]
