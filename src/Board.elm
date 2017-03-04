module Board
    exposing
        ( Model
        , Position
        , init
        , insert
        , positions
        , RunFilter
        , rays
        , filterRays
        , line
        , contiguousLines
        )

import List.Extra exposing (takeWhile)
import Coordinate.Hexagonal exposing (Coordinate, validWithin, squareOf)
import Direction exposing (Direction, directions, add)
import Dict exposing (Dict)


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


insert : Coordinate -> occupant -> Model occupant -> Model occupant
insert coordinate occupant model =
    Dict.update coordinate (Maybe.map (always occupant)) model


rays : Coordinate -> Model a -> List (List Coordinate)
rays =
    filterRays (always identity)


filterRays : RunFilter a -> Coordinate -> Model a -> List (List Coordinate)
filterRays filter origin model =
    directions
        |> List.map (ray origin [] model)
        |> List.map List.reverse
        |> List.map (filter model)


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


rayWithOrigin : Coordinate -> Run -> Model a -> Direction -> Run
rayWithOrigin origin acc model direction =
    let
        next =
            add direction origin
    in
        case Dict.get next model of
            Nothing ->
                origin :: acc

            Just _ ->
                rayWithOrigin next (origin :: acc) model direction


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


contiguousLines : Coordinate -> Model a -> List Run
contiguousLines origin model =
    directions
        |> List.map (\dir -> contiguousLine origin dir model)


contiguousLine : Coordinate -> Direction -> Model a -> Run
contiguousLine origin direction model =
    let
        isHead head coordinate =
            case Dict.get coordinate model of
                Nothing ->
                    False

                Just occupant ->
                    occupant == head
    in
        case Dict.get origin model of
            Nothing ->
                []

            Just head ->
                rayWithOrigin origin [] model direction
                    |> List.reverse
                    |> takeWhile (isHead head)
