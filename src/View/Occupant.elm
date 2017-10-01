module View.Occupant exposing (view)

import Svg exposing (Svg, Attribute)
import Coordinate.Hexagonal exposing (toCartesian)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , x1
        , x2
        , y1
        , y2
        , fill
        , fillOpacity
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
        ( x, y ) =
            toCartesian 2 coordinate

        baseAttrs : List (Svg.Attribute msg)
        baseAttrs =
            [ cx (toString x)
            , cy (toString y)
            ]

        makeCircle : List (Svg.Attribute msg) -> Svg msg
        makeCircle attrs =
            Svg.circle (baseAttrs ++ attrs) []

        subject =
            case Board.Occupant.toMaybe occupant of
                Nothing ->
                    makeCircle []

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
    in
        if shouldHighlight then
            Svg.g []
                [ lines ( x, y )
                , subject
                , highlight baseAttrs
                ]
        else
            Svg.g []
                [ lines ( x, y )
                , subject
                ]


highlight : List (Svg.Attribute msg) -> Svg msg
highlight baseAttrs =
    Svg.circle
        (baseAttrs
            ++ [ r "3%"
               , fill "yellow"
               , fillOpacity ".4"
               ]
        )
        []


lines : ( Float, Float ) -> Svg msg
lines ( x, y ) =
    let
        line x_ y_ =
            Svg.line
                [ x1 (toString (x - x_))
                , y1 (toString (y - y_))
                , x2 (toString (x + x_))
                , y2 (toString (y + y_))
                , stroke "darkslategrey"
                , strokeWidth "0.3%"
                ]
                []
    in
        Svg.g
            []
            [ line 0 1
            , line ((sqrt 3) / 2) (1 / 2)
            , line ((sqrt 3) / -2) (1 / 2)
            ]


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
