module Occupant
    exposing
    -- TODO: try not to export these constructors
        ( Occupant(..)
        , empty
        , ring
        , disc
        , isEmpty
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
