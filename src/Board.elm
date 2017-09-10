module Board
    exposing
        ( Board
        , init
        , toList
        , filter
        , add
        , emptyPositions
        , raysFrom
        )

import Dict exposing (Dict)
import Coordinate.Hexagonal as Hex exposing (Coordinate)
import Board.Occupant as Occupant exposing (Occupant)


type alias Board player marker =
    Model player marker


type alias Model player marker =
    Dict Coordinate (Occupant player marker)


type alias Position player marker =
    ( Int, Int, Occupant player marker )


init : Float -> Model player marker
init radius =
    Hex.squareOf (ceiling radius)
        |> List.filterMap (Hex.maybeValid radius)
        |> List.map (flip (,) Occupant.empty)
        |> Dict.fromList


add : Coordinate -> player -> marker -> Model player marker -> Model player marker
add coordinate player marker model =
    Dict.insert coordinate (Occupant.occupied player marker) model


toList : Model player marker -> List ( Int, Int, Maybe ( player, marker ) )
toList model =
    let
        toTuple : Coordinate -> Occupant player marker -> ( Int, Int, Maybe ( player, marker ) )
        toTuple ( x, y ) v =
            ( x, y, Occupant.toMaybe v )
    in
        Dict.map toTuple model |> Dict.values


rayFrom : Coordinate -> Coordinate -> Board player marker -> List (Position player marker)
rayFrom origin ( dx, dy ) model =
    let
        recRay : Coordinate -> List (Position player marker) -> List (Position player marker)
        recRay ( x, y ) acc =
            case Dict.get ( x, y ) model of
                Nothing ->
                    acc

                Just occupant ->
                    recRay ( x + dx, y + dy ) (( x, y, occupant ) :: acc)
    in
        recRay origin []


raysFrom : Coordinate -> Model player marker -> List (List (Position player marker))
raysFrom coordinate model =
    let
        directions =
            [ ( 1, 1 ), ( 1, 0 ), ( 0, 1 ), ( 0, -1 ), ( -1, 0 ), ( -1, -1 ) ]
    in
        directions
            |> List.map (\direction -> rayFrom coordinate direction model)


emptyPositions : Model player marker -> List Coordinate
emptyPositions model =
    Dict.filter (always <| (==) Occupant.empty) model |> Dict.keys


filter : (Coordinate -> player -> marker -> Bool) -> Model player marker -> Model player marker
filter f dict =
    let
        f_ coordinate occupant =
            Occupant.toMaybe occupant
                |> Maybe.map (\( player, marker ) -> f coordinate player marker)
                |> Maybe.withDefault False
    in
        Dict.filter f_ dict
