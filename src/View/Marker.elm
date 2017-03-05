module View.Marker exposing (toSvg)

import Svg exposing (Svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , fill
        , strokeWidth
        , stroke
        )
import Coordinate.Hexagonal as Coordinate
    exposing
        ( Coordinate
        , toCartesian
        )
import Player exposing (Player)
import Occupant exposing (Occupant)


toSvg : Coordinate -> Occupant -> Svg Never
toSvg coordinate occupant =
    let
        circle_ =
            case occupant of
                Occupant.Ring player ->
                    circle "4%" "none" [ stroke (Player.toColor player), strokeWidth "1%" ]

                Occupant.Disc player ->
                    circle "2%" (Player.toColor player) []

                Occupant.Empty ->
                    circle "0" "none" []
    in
        circle_ coordinate


circle : String -> String -> List (Svg.Attribute Never) -> Coordinate -> Svg Never
circle radius fill_ attrs coordinate =
    let
        ( x, y ) =
            coordinate |> toCartesian 2 |> Coordinate.toString
    in
        Svg.circle ([ cx x, cy y, r radius, fill fill_ ] ++ attrs) []
