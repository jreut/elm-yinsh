module Board.Occupant
    exposing
        ( Occupant
        , empty
        , occupied
        , toMaybe
        , update
        , isEmpty
        )

import Tuple exposing (first, second)


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


update : (( player, marker ) -> ( player, marker )) -> Occupant player marker -> Occupant player marker
update f occupant =
    case occupant of
        Empty ->
            Empty

        Occupied player marker ->
            let
                tuple =
                    f ( player, marker )
            in
                Occupied (first tuple) (second tuple)


isEmpty : Occupant player marker -> Bool
isEmpty occupant =
    case occupant of
        Empty ->
            True

        _ ->
            False
