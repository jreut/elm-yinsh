module Board
    exposing
        ( Board
        , init
        , toList
        , add
        , emptyPositions
        )

import Dict exposing (Dict)
import Coordinate.Hexagonal as Hex exposing (Coordinate)
import Board.Occupant as Occupant exposing (Occupant)


type alias Board player marker =
    Model player marker


type alias Model player marker =
    Dict Coordinate (Occupant player marker)


init : Float -> Model player marker
init radius =
    Hex.squareOf (ceiling radius)
        |> List.filterMap (Hex.maybeValid radius)
        |> List.map (flip (,) Occupant.empty)
        |> Dict.fromList


add : Int -> Int -> player -> marker -> Model player marker -> Model player marker
add x y player marker model =
    Dict.insert ( x, y ) (Occupant.occupied player marker) model


toList : Model player marker -> List ( Int, Int, Maybe ( player, marker ) )
toList model =
    let
        toTuple : Coordinate -> Occupant player marker -> ( Int, Int, Maybe ( player, marker ) )
        toTuple ( x, y ) v =
            ( x, y, Occupant.toMaybe v )
    in
        Dict.map toTuple model |> Dict.values


emptyPositions : Model player marker -> List ( Int, Int )
emptyPositions model =
    Dict.filter (\_ v -> v == Occupant.empty) model |> Dict.keys
