module View.Occupant exposing (view)

import Svg exposing (Svg, Attribute)
import Coordinate.Hexagonal exposing (toCartesian)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , fill
        , stroke
        , strokeWidth
        )
import Player exposing (Player(White, Black))
import Marker exposing (Marker(Disc, Ring))
import Board exposing (Position)
import Board.Occupant exposing (Occupant)


view : Bool -> Position Player Marker -> Svg msg
view shouldHighlight { coordinate, occupant } =
    let
        cartesian : ( Float, Float )
        cartesian =
            toCartesian 1 coordinate

        toCoordinate : Int -> String
        toCoordinate =
            toString << toFloat

        baseAttrs : List (Svg.Attribute msg)
        baseAttrs =
            let
                ( x, y ) =
                    cartesian
            in
                [ cx (toString x)
                , cy (toString y)
                ]

        emptyAttrs : List (Svg.Attribute msg)
        emptyAttrs =
            [ r "1%", fill "grey" ]

        ringAttrs : String -> List (Svg.Attribute msg)
        ringAttrs color =
            [ r "4%"
            , fill "none"
            , stroke color
            , strokeWidth "1%"
            ]

        discAttrs : String -> List (Svg.Attribute msg)
        discAttrs color =
            [ r "3%", fill color ]

        makeCircle : List (Svg.Attribute msg) -> Svg msg
        makeCircle attrs =
            Svg.circle (baseAttrs ++ attrs) []
    in
        case Board.Occupant.toMaybe occupant of
            Nothing ->
                makeCircle emptyAttrs

            Just ( player, marker ) ->
                let
                    makeAttrs =
                        case marker of
                            Disc ->
                                discAttrs

                            Ring ->
                                ringAttrs
                in
                    player
                        |> toString
                        |> String.toLower
                        |> makeAttrs
                        |> makeCircle
