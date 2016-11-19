module Player
    exposing
        ( Player
        , init
        , update
        , view
        )


type Player
    = White
    | Black


init : Player
init =
    White


update : Player -> Player
update player =
    case player of
        White ->
            Black

        Black ->
            White


view : Player -> String
view player =
    case player of
        Black ->
            "black"

        White ->
            "white"
