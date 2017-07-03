module View.Board
    exposing
        ( view
        , toSvg
        )

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox, width, height)
import Svg.Events exposing (onClick)
import Html exposing (Html)


type Config datum msg
    = JustSvg (datum -> Svg msg)
    | SvgWithMsg
        { toSvg : datum -> Svg msg
        , toMsg : datum -> msg
        }


toSvg : (datum -> Svg msg) -> Config datum msg
toSvg =
    JustSvg


view : Config datum msg -> List datum -> Html msg
view config data =
    let
        container =
            svg
                [ viewBox "-6 -6 12 12"
                , height "100vh"
                , width "100vw"
                ]
    in
        case config of
            JustSvg toSvg ->
                data |> List.map toSvg |> container

            SvgWithMsg { toSvg, toMsg } ->
                let
                    makeNode datum =
                        Svg.g [ onClick (toMsg datum) ] [ toSvg datum ]
                in
                    data |> List.map makeNode |> container
