module View.Board
    exposing
        ( view
        , config
        )

import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (viewBox, style)
import Svg.Events exposing (onClick)
import Html exposing (Html)


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
                [ viewBox "-6 -6 12 12"
                , style "flex-basis: 70%"
                ]

        makeNode datum =
            g [ onClick (toMsg datum) ] [ toSvg datum ]
    in
        data |> List.map makeNode |> container
