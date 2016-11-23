module Direction
    exposing
        ( Direction
        , directions
        , add
        )

import Coordinate.Hexagonal exposing (Coordinate)


type Direction
    = Up
    | Down
    | Left
    | Right
    | In
    | Out


directions : List Direction
directions =
    [ Up, Down, Left, Right, In, Out ]


add : Direction -> Coordinate -> Coordinate
add direction ( x, y ) =
    let
        ( dx, dy ) =
            vector direction
    in
        ( x + dx, y + dy )


vector : Direction -> Coordinate
vector direction =
    case direction of
        Up ->
            ( 0, 1 )

        Down ->
            ( 0, -1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )

        In ->
            ( 1, 1 )

        Out ->
            ( -1, -1 )
