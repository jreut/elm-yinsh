module View.Score exposing (view)

import Marker exposing (Marker(..))
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, style, viewBox, width)


scoreMarkers : Int -> String -> Int -> List (Svg msg)
scoreMarkers score color y =
    let
        indices =
            List.range 1 score
    in
    List.map
        (\x ->
            Marker.view "5.5%" ( toFloat x, toFloat y ) color Ring
        )
        indices


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
