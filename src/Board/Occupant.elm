module Board.Occupant
    exposing
        ( Occupant
        , empty
        , occupied
        , toMaybe
        , isEmpty
        )


type Occupant player marker
    = Occupied player marker
    | Empty


empty : Occupant player marker
empty =
    Empty


occupied : player -> marker -> Occupant player marker
occupied =
    Occupied


toMaybe : Occupant player marker -> Maybe ( player, marker )
toMaybe occupant =
    case occupant of
        Empty ->
            Nothing

        Occupied player marker ->
            Just ( player, marker )


isEmpty : Occupant player marker -> Bool
isEmpty occupant =
    case occupant of
        Empty ->
            True

        _ ->
            False
