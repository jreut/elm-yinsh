module Main exposing (..)

import Html


type alias Coordinate =
    ( Int, Int )


type alias Board =
    List Coordinate


{-| Directions in which one may traverse
-}
type Direction
    = Up
    | Down
    | Left
    | Right
    | In
    | Out


radius : Float
radius =
    4.6


init : Board
init =
    let
        roundedRadius =
            ceiling radius

        range =
            List.range (negate roundedRadius) roundedRadius
    in
        range
            |> List.concatMap (\x -> range |> List.map (\y -> ( x, y )))
            |> List.filter valid


square : number -> number
square n =
    n * n


valid : Coordinate -> Bool
valid ( x, y ) =
    let
        x_ =
            toFloat x

        y_ =
            toFloat y
    in
        (square radius) >= (square ((sqrt 3) / 2 * x_)) + (square ((x_ / 2) - y_))


add : Direction -> Coordinate -> Coordinate
add direction ( x, y ) =
    let
        vec =
            vector direction

        dx =
            Tuple.first vec

        dy =
            Tuple.second vec
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
