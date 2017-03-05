module Board
    exposing
        ( Board
        , Position
        , empty
        , toList
        , coordinate
        , occupant
        , insert
        , ringAt
        , isEmpty
        )

import Dict exposing (Dict)
import Occupant exposing (Occupant)
import Player exposing (Player)
import Coordinate.Hexagonal
    exposing
        ( Coordinate
        , maybeValid
        , squareOf
        )


type Board
    = Board (Dict Coordinate (Occupant Player))


type Position
    = Position ( Coordinate, Occupant Player )


fromList : List ( Coordinate, Occupant Player ) -> Board
fromList =
    Board << Dict.fromList


fromDict : Dict Coordinate (Occupant Player) -> Board
fromDict =
    Board


empty : Board
empty =
    let
        makePair : Coordinate -> Maybe ( Coordinate, Occupant Player )
        makePair =
            Maybe.map (flip (,) Occupant.empty) << maybeValid radius

        radius =
            4.6
    in
        squareOf (ceiling radius)
            |> List.filterMap makePair
            |> fromList


toList : Board -> List Position
toList (Board board) =
    Dict.toList board
        |> List.map Position


coordinate : Position -> Coordinate
coordinate (Position ( coordinate, _ )) =
    coordinate


occupant : Position -> Occupant Player
occupant (Position ( _, occupant )) =
    occupant


insert : Coordinate -> Occupant Player -> Board -> Board
insert coordinate occupant (Board board) =
    Dict.insert coordinate occupant board |> Board


ringAt : Coordinate -> Player -> Board -> Board
ringAt coordinate player =
    insert coordinate (Occupant.ring player)


isEmpty : Coordinate -> Board -> Bool
isEmpty coordinate (Board board) =
    Dict.get coordinate board |> Maybe.map Occupant.isEmpty |> Maybe.withDefault True
