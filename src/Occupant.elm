module Occupant
    exposing
    -- TODO: try not to export these constructors
        ( Occupant(..)
        , empty
        , ring
        , disc
        , isEmpty
        , isRing
        , isDisc
        , toPlayer
        )

import Player exposing (Player)


type Occupant
    = Ring Player
    | Disc Player
    | Empty


empty : Occupant
empty =
    Empty


ring : Player -> Occupant
ring =
    Ring


disc : Player -> Occupant
disc =
    Disc


isEmpty : Occupant -> Bool
isEmpty =
    (==) Empty


isRing : Occupant -> Bool
isRing occupant =
    case occupant of
        Ring _ ->
            True

        _ ->
            False


isDisc : Occupant -> Bool
isDisc occupant =
    case occupant of
        Disc _ ->
            True

        _ ->
            False


toPlayer : Occupant -> Maybe Player
toPlayer occupant =
    case occupant of
        Ring player ->
            Just player

        Disc player ->
            Just player

        Empty ->
            Nothing
