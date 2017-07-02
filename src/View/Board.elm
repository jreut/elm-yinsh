module View.Board
    exposing
        ( view
        , toSvg
        )

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox, width, height)
import Html exposing (Html)


type Config datum
    = JustSvg (datum -> Svg Never)


toSvg : (datum -> Svg Never) -> Config datum
toSvg =
    JustSvg


view : Config datum -> List datum -> Html Never
view config data =
    case config of
        JustSvg toSvg ->
            data
                |> List.map toSvg
                |> svg
                    [ viewBox "-6 -6 12 12"
                    , height "100vh"
                    , width "100vw"
                    ]
