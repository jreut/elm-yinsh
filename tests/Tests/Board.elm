module Tests.Board exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (intRange)
import Test exposing (..)
import Set exposing (Set)
import Dict
import Board
    exposing
        ( Board
        , init
        , add
        , raysFrom
        , updateBetween
        , emptyPositions
        )
import Board.Occupant as Occupant
import Coordinate.Hexagonal exposing (Coordinate)


smallBoard =
    init 1


smallBoardPositions =
    (Set.fromList
        [ ( 1, 1 )
        , ( 1, 0 )
        , ( 0, 1 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( -1, 0 )
        , ( -1, -1 )
        ]
    )


raysOfCoordinates : Coordinate -> Board player occupant -> Set (List Coordinate)
raysOfCoordinates origin model =
    raysFrom origin model
        |> List.map (List.map (.coordinate))
        |> Set.fromList


suite : Test
suite =
    describe "Board"
        [ describe "raysFrom"
            [ describe "the middle (0,0)"
                [ test "are the expected values" <|
                    \_ ->
                        raysOfCoordinates ( 0, 0 ) smallBoard
                            |> Expect.equal
                                (smallBoardPositions
                                    |> Set.remove ( 0, 0 )
                                    |> Set.map List.singleton
                                )
                ]
            , describe "an edge (-1,-1)"
                [ test "are the expected values" <|
                    \_ ->
                        raysOfCoordinates ( -1, -1 ) smallBoard
                            |> Expect.equal
                                (Set.fromList
                                    [ [ ( 0, 0 ), ( 1, 1 ) ]
                                    , [ ( 0, -1 ) ]
                                    , [ ( -1, 0 ) ]
                                    , []
                                    , []
                                    , []
                                    ]
                                )
                ]
            ]
        , describe "emptyPositions"
            [ describe "of an empty board"
                [ test "are all the positions" <|
                    \_ ->
                        emptyPositions smallBoard
                            |> Expect.equal smallBoardPositions
                ]
            , describe "after adding a single occupant"
                [ test "are all the positions except that one" <|
                    \_ ->
                        smallBoard
                            |> add ( 0, 0 ) "player" "marker"
                            |> emptyPositions
                            |> Expect.equal (smallBoardPositions |> Set.remove ( 0, 0 ))
                ]
            ]
        , describe "updateBetween"
            [ test "updates each occupant in between" <|
                \_ ->
                    let
                        board =
                            smallBoard
                                |> add ( 0, 1 ) "white" "disc"
                                |> add ( 0, 0 ) "white" "disc"
                                |> add ( 0, -1 ) "white" "ring"

                        updater occupant =
                            case Occupant.toMaybe occupant of
                                Nothing ->
                                    Occupant.empty

                                Just ( player, marker ) ->
                                    Occupant.occupied "black" marker
                    in
                        updateBetween updater ( 0, 1 ) ( 0, -1 ) board
                            |> Expect.equal (board |> add ( 0, 0 ) "black" "disc")
            ]
        ]
