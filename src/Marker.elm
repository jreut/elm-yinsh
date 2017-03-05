module Marker
    exposing
        ( Marker
        , ring
        , disc
        , isRing
        , isDisc
        )


type Marker player
    = Ring player
    | Disc player


ring : player -> Marker player
ring player =
    Ring player


disc : player -> Marker player
disc player =
    Disc player


isRing : Marker a -> Bool
isRing marker =
    case marker of
        Ring _ ->
            True

        _ ->
            False


isDisc : Marker a -> Bool
isDisc marker =
    case marker of
        Disc _ ->
            True

        _ ->
            False
