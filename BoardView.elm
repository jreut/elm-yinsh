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
            , fontSize "5%"
            ]


viewPosition : Config data msg -> State -> data -> Svg msg
viewPosition { toCoordinate, toMsg, toSvg } state data =
    let
        coordinate =
            toCoordinate data
    in
        g []
            [ viewLines coordinate
            , toSvg data
            , circle
                [ cx (Tuple.first coordinate |> toString)
                , cy (Tuple.second coordinate |> toString)
                , r "3%"
                , fill "none"
                , pointerEvents "all"
                , onClick (toMsg state data)
                ]
                []
            ]


viewLines : Coordinate -> Svg msg
viewLines ( x, y ) =
    let
        line =
            \x_ y_ ->
                Svg.line
                    [ x1 (toString x)
                    , y1 (toString y)
                    , x2 (toString (x + x_))
                    , y2 (toString (y + y_))
                    , stroke "black"
                    , strokeWidth "0.3%"
                    ]
                    []
    in
        Svg.g
            []
            [ line 0 1
            , line 0 -1
            , line ((sqrt 3) / 2) (1 / 2)
            , line ((sqrt 3) / 2) (-1 / 2)
            , line ((sqrt 3) / -2) (-1 / 2)
            , line ((sqrt 3) / -2) (1 / 2)
            ]
