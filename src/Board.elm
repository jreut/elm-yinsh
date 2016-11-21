module Board
    exposing
        ( Model
        , Position
        , init
        , update
        , positions
        , RunFilter
        , filteredRuns
        )

import Coordinate.Hexagonal exposing (Coordinate, validWithin, squareOf)
import Dict exposing (Dict)
import Set exposing (Set)


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


type Direction
    = Up
    | Down
    | Left
    | Right
    | In
    | Out


type alias RunFilter a =
    Model a -> List Coordinate -> List Coordinate


filteredRuns : RunFilter a -> Coordinate -> Model a -> Set Coordinate
filteredRuns filter origin model =
    [ Up
    , Down
    , Left
    , Right
    , In
    , Out
    ]
        |> List.map (\dir -> runIn origin dir [] model)
        |> List.map List.reverse
        |> List.concatMap (filter model)
        |> Set.fromList


runIn : Coordinate -> Direction -> List Coordinate -> Model a -> List Coordinate
runIn origin direction acc model =
    let
        next =
            add direction origin
    in
        case Dict.get next model of
            Nothing ->
                acc

            Just _ ->
                runIn next direction (next :: acc) model


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
