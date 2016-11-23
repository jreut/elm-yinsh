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
        |> List.map (flip (,) initial)
        |> Dict.fromList


update : Coordinate -> a -> Model a -> Model a
update =
    Dict.insert


filteredRuns : RunFilter a -> Coordinate -> Model a -> Set Coordinate
filteredRuns filter origin model =
    directions
        |> List.map (ray origin [] model)
        |> List.map List.reverse
        |> List.concatMap (filter model)
        |> Set.fromList


ray : Coordinate -> Run -> Model a -> Direction -> Run
ray origin acc model direction =
    let
        next =
            add direction origin
    in
        case Dict.get next model of
            Nothing ->
                acc

            Just _ ->
                ray next (next :: acc) model direction


line : Coordinate -> Coordinate -> Model a -> Run
line origin destination =
    runTo origin destination []


runTo : Coordinate -> Coordinate -> Run -> Model a -> Run
runTo origin destination acc model =
    List.concatMap
        (tryRun origin destination [] model)
        directions


tryRun : Coordinate -> Coordinate -> Run -> Model a -> Direction -> Run
tryRun origin destination acc model direction =
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
                    tryRun next destination (next :: acc) model direction
