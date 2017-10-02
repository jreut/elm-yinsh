module View.Score exposing (view)

import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, style, viewBox, width)


scoreMarker : String -> Int -> Int -> Svg msg
scoreMarker color y x =
    let
        baseAttrs : List (Svg.Attribute msg)
        baseAttrs =
            [ cx (toString x)
            , cy (toString y)
            , r "5.5%"
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
    circle baseAttrs []


scoreMarkers : Int -> String -> Int -> List (Svg msg)
scoreMarkers score color y =
    if score > 0 then
        let
            indices =
                List.drop 1 (List.range 0 score)
        in
        List.map
            (scoreMarker color y)
            indices
    else
        []


view : { black : Int, white : Int } -> Svg msg
view scores =
    let
        { black, white } =
            scores
    in
    svg
        [ viewBox "-0.5 0.5 10 5" ]
        [ Svg.g [] (scoreMarkers black "black" 1)
        , Svg.g [] (scoreMarkers white "white" 3)
        ]
