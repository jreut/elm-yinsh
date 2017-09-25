module Tests.Board exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (intRange)
import Test exposing (..)
import Set exposing (Set)
import Dict
import Board


smallBoard =
    Board.init 1


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


raysOfCoordinates : ( Int, Int ) -> Board.Board player occupant -> Set (List ( Int, Int ))
raysOfCoordinates origin model =
    Board.raysFrom origin model
        |> List.map (List.map (\( x, y, _ ) -> ( x, y )))
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
                                    |> Set.map (\( f, s ) -> [ ( 0, 0 ), ( f, s ) ])
                                )
                ]
            , describe "an edge (-1,-1)"
                [ test "are the expected values" <|
                    \_ ->
                        raysOfCoordinates ( -1, -1 ) smallBoard
                            |> Expect.equal
                                (Set.fromList
                                    [ [ ( -1, -1 ), ( 0, 0 ), ( 1, 1 ) ]
                                    , [ ( -1, -1 ), ( 0, -1 ) ]
                                    , [ ( -1, -1 ), ( -1, 0 ) ]
                                    , [ ( -1, -1 ) ]
                                    , [ ( -1, -1 ) ]
                                    , [ ( -1, -1 ) ]
                                    ]
                                )
                ]
            ]
        , describe "emptyPositions"
            [ describe "of an empty board"
                [ test "are all the positions" <|
                    \_ ->
                        Board.emptyPositions smallBoard
                            |> Expect.equal smallBoardPositions
                ]
            , describe "after adding a single occupant"
                [ test "are all the positions except that one" <|
                    \_ ->
                        smallBoard
                            |> Board.add ( 0, 0 ) "player" "marker"
                            |> Board.emptyPositions
                            |> Expect.equal (smallBoardPositions |> Set.remove ( 0, 0 ))
                ]
            ]
        ]
