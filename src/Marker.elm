module Marker exposing (Marker, ring, disc)


type Marker player
    = Ring player
    | Disc player


ring : player -> Marker player
ring player =
    Ring player


disc : player -> Marker player
disc player =
    Disc player
