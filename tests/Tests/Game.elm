module Tests.Game exposing (suite)

import Test exposing (Test, describe, test, fuzz2)
import Fuzz exposing (int)
import Expect
import Game
    exposing
        ( jumpCoordinate
        , freedomsForRay
        )
import Board.Occupant exposing (Occupant, occupied, empty)
import Marker exposing (Marker(Ring, Disc))
import Player exposing (Player(White, Black))


suite : Test
suite =
    describe "Game"
        [ describe "jumpCoordinate"
            [ test "with an empty list" <|
                \_ ->
                    jumpCoordinate []
                        |> Expect.equal Nothing
            , fuzz2 int int "with a single empty space" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, empty ) ]
                        |> Expect.equal (Just ( x, y, empty ))
            , fuzz2 int int "with a single ring" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, occupied White Ring ) ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a single disc" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, occupied White Disc ) ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a ring then an empty space" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, occupied White Ring )
                        , ( x + 1, y + 1, empty )
                        ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a disc then an empty space" <|
                \x y ->
                    let
                        emptyOne =
                            ( x + 1, y + 1, empty )
                    in
                        jumpCoordinate
                            [ ( x, y, occupied White Disc )
                            , emptyOne
                            ]
                            |> Expect.equal (Just emptyOne)
            , fuzz2 int int "with two discs then an empty space" <|
                \x y ->
                    let
                        emptyOne =
                            ( x + 1, y + 1, empty )
                    in
                        jumpCoordinate
                            [ ( x, y, occupied White Disc )
                            , ( x + 1, y + 1, occupied Black Disc )
                            , emptyOne
                            ]
                            |> Expect.equal (Just emptyOne)
            , fuzz2 int int "with two discs, a ring, and then an empty space" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, occupied White Disc )
                        , ( x + 1, y + 1, occupied White Disc )
                        , ( x + 2, y + 2, occupied White Ring )
                        , ( x + 3, y + 3, empty )
                        ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a Disc and two empty spaces" <|
                \x y ->
                    jumpCoordinate
                        [ ( x, y, occupied White Disc )
                        , ( x + 1, y + 1, empty )
                        , ( x + 2, y + 2, empty )
                        ]
                        |> Expect.equal (Just ( x + 1, y + 1, empty ))
            ]
        , describe "freedomsForRay"
            [ test "with the empty list" <|
                \_ -> freedomsForRay [] |> Expect.equal []
            , fuzz2 int int "with one empty space" <|
                \x y ->
                    freedomsForRay [ ( x, y, empty ) ]
                        |> Expect.equal [ ( x, y ) ]
            , fuzz2 int int "with an empty space, a disc, and another empty space" <|
                \x y ->
                    freedomsForRay
                        [ ( x, y, empty )
                        , ( x + 1, y + 1, occupied Black Disc )
                        , ( x + 2, y + 2, empty )
                        ]
                        |> Expect.equal [ ( x, y ), ( x + 2, y + 2 ) ]
            , fuzz2 int int "with an empty space, a ring, and another empty space" <|
                \x y ->
                    freedomsForRay
                        [ ( x, y, empty )
                        , ( x + 1, y + 1, occupied Black Ring )
                        , ( x + 2, y + 2, empty )
                        ]
                        |> Expect.equal [ ( x, y ) ]
            , fuzz2 int int "with a disc, a ring, and an empty space" <|
                \x y ->
                    freedomsForRay
                        [ ( x, y, occupied Black Disc )
                        , ( x + 1, y + 1, occupied Black Ring )
                        , ( x + 2, y + 2, empty )
                        ]
                        |> Expect.equal []
            ]
        ]
