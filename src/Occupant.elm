module Occupant
    exposing
        ( Occupant
        , empty
        , ring
        , disc
        , isEmpty
        )


type Occupant player
    = Ring player
    | Disc player
    | Empty


empty : Occupant player
empty =
    Empty


ring : player -> Occupant player
ring =
    Ring


disc : player -> Occupant player
disc =
    Disc


isEmpty : Occupant player -> Bool
isEmpty =
    (==) Empty
