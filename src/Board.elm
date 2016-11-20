module Board
    exposing
        ( Model
        , Position
        , init
        , update
        , positions
        , runsOf
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


runsOf : Coordinate -> a -> Model a -> Set Coordinate
runsOf origin occupant model =
    let
        runOf_ =
            \dir acc -> runOf origin dir occupant acc model
    in
        runOf_ Up Set.empty
            |> runOf_ Down
            |> runOf_ Left
            |> runOf_ Right
            |> runOf_ In
            |> runOf_ Out


runOf : Coordinate -> Direction -> a -> Set Coordinate -> Model a -> Set Coordinate
runOf origin direction desiredOccupant acc model =
    let
        next =
            add direction origin
    in
        case Dict.get next model of
            Nothing ->
                acc

            Just occupant_ ->
                if occupant_ == desiredOccupant then
                    runOf next direction desiredOccupant (Set.insert next acc) model
                else
                    acc


runIn : Coordinate -> Direction -> Set Coordinate -> Model a -> Set Coordinate
runIn origin direction acc model =
    let
        next =
            add direction origin
    in
        case Dict.get next model of
            Nothing ->
                acc

            Just _ ->
                runIn next direction (Set.insert next acc) model


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
