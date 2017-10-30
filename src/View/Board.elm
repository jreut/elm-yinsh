module View.Board
    exposing
        ( config
        , view
        )

import Html exposing (Html)
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (style, viewBox)
import Svg.Events exposing (onClick)


type Config datum msg
    = Config
        { toSvg : datum -> Svg msg
        , toMsg : datum -> msg
        }


config :
    { toSvg : datum -> Svg msg
    , toMsg : datum -> msg
    }
    -> Config datum msg
config =
    Config


view : Config datum msg -> List datum -> Html msg
view (Config { toSvg, toMsg }) data =
    let
        container =
            svg
                [ viewBox "-10 -10 20 20"
                , style "flex-basis: 60%"
                ]

        makeNode datum =
            g [ onClick (toMsg datum) ] [ toSvg datum ]
    in
    data |> List.map makeNode |> container
