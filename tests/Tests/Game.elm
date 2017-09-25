module Tests.Game exposing (suite)

import Test exposing (Test, describe, test, fuzz2)
import Fuzz exposing (int)
import Expect
import Game
    exposing
        ( jumpCoordinate
        , freedomsForRay
        )
import Board exposing (Position)
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
                    let
                        emptyOne =
                            { coordinate = ( x, y ), occupant = empty }
                    in
                        jumpCoordinate
                            [ emptyOne ]
                            |> Expect.equal (Just emptyOne)
            , fuzz2 int int "with a single ring" <|
                \x y ->
                    jumpCoordinate
                        [ positionFor x y (occupied White Ring) ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a single disc" <|
                \x y ->
                    jumpCoordinate
                        [ positionFor x y (occupied White Disc) ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a ring then an empty space" <|
                \x y ->
                    jumpCoordinate
                        [ positionFor x y (occupied White Ring)
                        , positionFor (x + 1) (y + 1) empty
                        ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a disc then an empty space" <|
                \x y ->
                    let
                        emptyOne =
                            positionFor (x + 1) (y + 1) empty
                    in
                        jumpCoordinate
                            [ positionFor x y (occupied White Disc)
                            , emptyOne
                            ]
                            |> Expect.equal (Just emptyOne)
            , fuzz2 int int "with two discs then an empty space" <|
                \x y ->
                    let
                        emptyOne =
                            positionFor (x + 1) (y + 1) empty
                    in
                        jumpCoordinate
                            [ positionFor x y (occupied White Disc)
                            , positionFor (x + 1) (y + 1) (occupied Black Disc)
                            , emptyOne
                            ]
                            |> Expect.equal (Just emptyOne)
            , fuzz2 int int "with two discs, a ring, and then an empty space" <|
                \x y ->
                    jumpCoordinate
                        [ positionFor x y (occupied White Disc)
                        , positionFor (x + 1) (y + 1) (occupied White Disc)
                        , positionFor (x + 2) (y + 2) (occupied White Ring)
                        , positionFor (x + 3) (y + 3) empty
                        ]
                        |> Expect.equal Nothing
            , fuzz2 int int "with a Disc and two empty spaces" <|
                \x y ->
                    let
                        wanted =
                            positionFor (x + 1) (y + 1) empty
                    in
                        jumpCoordinate
                            [ positionFor x y (occupied White Disc)
                            , wanted
                            , positionFor (x + 2) (y + 2) empty
                            ]
                            |> Expect.equal (Just wanted)
            ]
        , describe "freedomsForRay"
            [ test "with the empty list" <|
                \_ -> freedomsForRay [] |> Expect.equal []
            , fuzz2 int int "with one empty space" <|
                \x y ->
                    let
                        emptyOne =
                            positionFor x y empty
                    in
                        freedomsForRay [ emptyOne ]
                            |> Expect.equal [ emptyOne.coordinate ]
            , fuzz2 int int "with an empty space, a disc, and another empty space" <|
                \x y ->
                    let
                        firstEmpty =
                            positionFor x y empty

                        lastEmpty =
                            positionFor (x + 2) (y + 2) empty
                    in
                        freedomsForRay
                            [ firstEmpty
                            , positionFor (x + 1) (y + 1) (occupied Black Disc)
                            , lastEmpty
                            ]
                            |> Expect.equal
                                [ firstEmpty.coordinate
                                , lastEmpty.coordinate
                                ]
            , fuzz2 int int "with an empty space, a ring, and another empty space" <|
                \x y ->
                    let
                        firstEmpty =
                            positionFor x y empty
                    in
                        freedomsForRay
                            [ firstEmpty
                            , positionFor (x + 1) (y + 1) (occupied Black Ring)
                            , positionFor (x + 2) (y + 2) empty
                            ]
                            |> Expect.equal [ firstEmpty.coordinate ]
            , fuzz2 int int "with a disc, a ring, and an empty space" <|
                \x y ->
                    freedomsForRay
                        [ positionFor x y (occupied Black Disc)
                        , positionFor (x + 1) (y + 1) (occupied Black Ring)
                        , positionFor (x + 2) (y + 2) empty
                        ]
                        |> Expect.equal []
            ]
        ]


positionFor : Int -> Int -> Occupant player marker -> Position player marker
positionFor x y occupant =
    { coordinate = ( x, y ), occupant = occupant }
