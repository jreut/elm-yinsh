module Player
    exposing
        ( Player
        , white
        , black
        , next
        , toColor
        )


type Player
    = White
    | Black


white : Player
white =
    White


black : Player
black =
    Black


next : Player -> Player
next player =
    case player of
        White ->
            Black

        Black ->
            White


toColor : Player -> String
toColor player =
    case player of
        White ->
            "white"

        Black ->
            "black"
