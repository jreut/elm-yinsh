module View.Board
    exposing
        ( view
        , toSvgAndMsg
        )

import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (viewBox, width, height)
import Svg.Events exposing (onClick)
import Html exposing (Html)


type Config datum msg
    = SvgWithMsg
        { toSvg : datum -> Svg msg
        , toMsg : datum -> msg
        }


toSvgAndMsg : (datum -> Svg msg) -> (datum -> msg) -> Config datum msg
toSvgAndMsg toSvg toMsg =
    SvgWithMsg { toSvg = toSvg, toMsg = toMsg }


view : Config datum msg -> List datum -> Html msg
view (SvgWithMsg { toSvg, toMsg }) data =
    let
        container =
            svg
                [ viewBox "-6 -6 12 12"
                , height "80vh"
                , width "100vw"
                ]

        makeNode datum =
            g [ onClick (toMsg datum) ] [ toSvg datum ]
    in
        data |> List.map makeNode |> container
