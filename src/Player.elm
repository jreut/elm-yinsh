module Player
    exposing
        ( Player
        , white
        , black
        , next
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
