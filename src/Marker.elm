module Marker exposing (Marker(..), view)

import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, style, viewBox, width)


type Marker
    = Ring
    | Disc


view : String -> ( Float, Float ) -> String -> Marker -> Svg msg
view radius coordinate color marker =
    let
        ( x, y ) =
            coordinate

        baseAttrs : List (Svg.Attribute msg)
        baseAttrs =
            [ cx (toString x)
            , cy (toString y)
            , r radius
            , stroke color
            , fill "none"
            , strokeWidth "1.5%"
            , width "100"
            , height "100"
            ]

        makeCircle : List (Svg.Attribute msg) -> Svg msg
        makeCircle attrs =
            Svg.circle baseAttrs []
    in
    svg []
        [ Svg.circle
            baseAttrs
            []
        ]
