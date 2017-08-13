module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import Game


suite : Test
suite =
    describe "Game"
        [ describe ".message"
            [ test "initial" <|
                \_ ->
                    Game.message Game.init
                        |> Expect.equal
                            "White to place a ring (0 rings placed)"
            ]
        , describe ".availableMoves"
            [ describe "initial"
                [ test "are the entire board" <|
                    \_ ->
                        (Game.availableMoves Game.init |> List.length)
                            |> Expect.equal
                                (Game.board Game.init |> Dict.size)
                ]
            ]
        ]
