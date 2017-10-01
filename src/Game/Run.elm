module Game.Run
    exposing
        ( Run
        , init
        , player
        , coordinates
        , unique
        )

import Set exposing (Set)
import Dict exposing (Dict)
import Player exposing (Player)
import Coordinate.Hexagonal exposing (Coordinate)


type Run
    = Run Player (Set Coordinate)


init : Player -> Set Coordinate -> Run
init =
    Run


player : Run -> Player
player (Run player _) =
    player


coordinates : Run -> Set Coordinate
coordinates (Run _ coordinates) =
    coordinates


unique : List Run -> List Run
unique runs =
    List.foldl
        (\run acc ->
            if List.any (equal run) acc then
                acc
            else
                run :: acc
        )
        []
        runs


equal : Run -> Run -> Bool
equal (Run player coordinates) (Run player_ coordinates_) =
    (player == player_)
        && (Set.size coordinates == Set.size coordinates_)
        && (Set.intersect coordinates coordinates_ == coordinates)
