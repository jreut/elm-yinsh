module Board
    exposing
        ( Model
        , Position
        , init
        , update
        , positions
        )

import Coordinate.Hexagonal exposing (Coordinate, validWithin, squareOf)
import Dict exposing (Dict)


type alias Model a =
    Dict Coordinate a


type alias Position a =
    ( Coordinate, a )


init : Float -> a -> Model a
init radius initial =
    squareOf (ceiling radius)
        |> List.filter (validWithin radius)
        |> List.map (\e -> ( e, initial ))
        |> Dict.fromList


update : Coordinate -> a -> Model a -> Model a
update =
    Dict.insert


positions : Model a -> List (Position a)
positions =
    Dict.toList
