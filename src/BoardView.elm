module BoardView
    exposing
        ( State
        , Config
        , Coordinate
        , view
        , init
        )

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias State =
    ()


type alias Coordinate =
    ( Float, Float )


type alias Config data msg =
    { toCoordinate : data -> Coordinate
    , toMsg : State -> data -> msg
    , toSvg : data -> Svg msg
    }


init : State
init =
    ()


view : Config data msg -> State -> List data -> Svg msg
view config state data =
    data
        |> List.map (viewPosition config state)
        |> svg
            [ viewBox "-10 -10 20 20"
            , height "100vh"
            , width "100vw"
            ]


viewPosition : Config data msg -> State -> data -> Svg msg
viewPosition { toCoordinate, toMsg, toSvg } state data =
    let
        coordinate =
            toCoordinate data
    in
        g []
            [ viewBackground coordinate
            , toSvg data
            , viewForeground coordinate (toMsg state data)
            ]


viewForeground : Coordinate -> msg -> Svg msg
viewForeground ( x, y ) msg =
    circle
        [ cx (toString x)
        , cy (toString y)
        , r "3%"
        , fill "none"
        , pointerEvents "all"
        , onClick msg
        ]
        []


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
