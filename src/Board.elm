module Board
    exposing
        ( Model
        , Position
        , init
        , update
        , positions
        , RunFilter
        , filteredRuns
        , line
        )

import Coordinate.Hexagonal exposing (Coordinate, validWithin, squareOf)
import Direction exposing (Direction, directions, add)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Model a =
    Dict Coordinate a


type alias Position a =
    ( Coordinate, a )


positions : Model a -> List (Position a)
positions =
    Dict.toList


type alias Run =
    List Coordinate


type alias RunFilter a =
    Model a -> Run -> Run


init : Float -> a -> Model a
init radius initial =
    squareOf (ceiling radius)
        |> List.filter (validWithin radius)
        |> List.map (\e -> ( e, initial ))
        |> Dict.fromList


update : Coordinate -> a -> Model a -> Model a
update =
    Dict.insert


filteredRuns : RunFilter a -> Coordinate -> Model a -> Set Coordinate
filteredRuns filter origin model =
    directions
        |> List.map (\dir -> runIn origin dir [] model)
        |> List.map List.reverse
        |> List.concatMap (filter model)
        |> Set.fromList


runIn : Coordinate -> Direction -> Run -> Model a -> Run
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


line : Coordinate -> Coordinate -> Model a -> Run
line origin destination =
    runTo origin destination []


runTo : Coordinate -> Coordinate -> Run -> Model a -> Run
runTo origin destination acc model =
    List.concatMap
        (\dir -> tryRun origin destination dir [] model)
        directions


tryRun : Coordinate -> Coordinate -> Direction -> Run -> Model a -> Run
tryRun origin destination direction acc model =
    let
        next =
            add direction origin
    in
        if next == destination then
            next :: acc
        else
            case Dict.get next model of
                Nothing ->
                    []

                Just _ ->
                    tryRun next destination direction (next :: acc) model
