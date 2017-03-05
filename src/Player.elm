module Player
    exposing
        ( Player
        , init
        , next
        , view
        )


type Player
    = White
    | Black


init : Player
init =
    White


next : Player -> Player
next player =
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
