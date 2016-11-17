module Player
    exposing
        ( Model
        , init
        , update
        , view
        )


type Model
    = White
    | Black


init : Model
init =
    White


update : Model -> Model
update player =
    case player of
        White ->
            Black

        Black ->
            White


view : Model -> String
view player =
    case player of
        Black ->
            "black"

        White ->
            "white"
