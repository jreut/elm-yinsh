module View.Board
    exposing
        ( view
        , toSvg
        )

import Svg exposing (Svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , viewBox
        , width
        , height
        )
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
            data |> List.map toSvg |> Svg.svg []
