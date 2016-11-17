module Board
    exposing
        ( Model
        , init
        , update
        )

import Coordinate exposing (Coordinate, validWithin, squareOf)
import Dict exposing (Dict)


type alias Model a =
    Dict Coordinate a


type alias Coordinate =
    ( Int, Int )


init : Float -> a -> Model a
init radius initial =
    squareOf (ceiling radius)
        |> List.filter (validWithin radius)
        |> List.map (\e -> ( e, initial ))
        |> Dict.fromList


update : Coordinate -> a -> Model a -> Model a
update =
    Dict.insert
