module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
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
        ]
