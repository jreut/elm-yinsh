module Board.Occupant
    exposing
        ( Occupant
        , empty
        , occupied
        , toMaybe
        , update
        , mapWithDefault
        , isEmpty
        )

import Tuple exposing (first, second)


type Occupant player marker
    = Occupied player marker
    | Empty


{-| Construct an empty occupant
-}
empty : Occupant player marker
empty =
    Empty


{-| Construct an occupant of the given player and marker
-}
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


{-| Update an occupant's player or marker. Empty occupants are left unmodified
-}
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


{-| Map over an occupant, providing a default value for empty occupants.
-}
mapWithDefault : a -> (( player, marker ) -> a) -> Occupant player marker -> a
mapWithDefault default f occupant =
    case occupant of
        Empty ->
            default

        Occupied player marker ->
            f ( player, marker )


{-| Determine whether an occupant is empty.
-}
isEmpty : Occupant player marker -> Bool
isEmpty occupant =
    case occupant of
        Empty ->
            True

        _ ->
            False
