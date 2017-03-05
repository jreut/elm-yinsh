module View.Board
    exposing
        ( Config
        , config
        , Coordinate
        , view
        )

import Svg exposing (Svg, svg, circle, line, g)
import Svg.Attributes
    exposing
        ( viewBox
        , height
        , width
        , fill
        , fillOpacity
        , stroke
        , strokeWidth
        , pointerEvents
        , cx
        , cy
        , r
        , x1
        , y1
        , x2
        , y2
        )
import Svg.Events exposing (onClick)


type alias Coordinate =
    ( Float, Float )


type Config data msg
    = Config
        { toCoordinate : data -> Coordinate
        , toMsg : data -> Maybe msg
        , toSvg : data -> Svg msg
        }


config : (data -> Coordinate) -> (data -> Maybe msg) -> (data -> Svg msg) -> Config data msg
config toCoordinate toMsg toSvg =
    Config
        { toCoordinate = toCoordinate
        , toMsg = toMsg
        , toSvg = toSvg
        }


view : Config data msg -> List data -> Svg msg
view config data =
    data
        |> List.map (viewPosition config)
        |> svg
            [ viewBox "-10 -10 20 20"
            , height "100vh"
            , width "100vw"
            ]


viewPosition : Config data msg -> data -> Svg msg
viewPosition (Config { toCoordinate, toMsg, toSvg }) data =
    let
        coordinate =
            toCoordinate data
    in
        g []
            [ viewBackground coordinate
            , toSvg data
            , viewForeground coordinate (toMsg data)
            ]


viewForeground : Coordinate -> Maybe msg -> Svg msg
viewForeground ( x, y ) maybeMsg =
    let
        ( onClick_, opacity ) =
            case maybeMsg of
                Nothing ->
                    ( [], "0" )

                Just msg ->
                    ( [ onClick msg ], "0.3" )

        attrs =
            [ cx (toString x)
            , cy (toString y)
            , r "3%"
            , fill "yellow"
            , fillOpacity opacity
            , pointerEvents "all"
            ]
    in
        circle (attrs ++ onClick_) []


viewBackground : Coordinate -> Svg msg
viewBackground =
    viewLines


viewLines : Coordinate -> Svg msg
viewLines coordinate =
    [ ( 0, 1 )
    , ( (sqrt 3) / 2, 1 / 2 )
    , ( (sqrt 3) / -2, 1 / 2 )
    ]
        |> List.map (viewLine coordinate)
        |> g []


viewLine : Coordinate -> Coordinate -> Svg msg
viewLine ( x, y ) ( x_, y_ ) =
    line
        [ x1 (toString (x - x_))
        , y1 (toString (y - y_))
        , x2 (toString (x + x_))
        , y2 (toString (y + y_))
        , stroke "black"
        , strokeWidth "0.3%"
        ]
        []
