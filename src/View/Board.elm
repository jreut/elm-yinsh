module View.Board
    exposing
        ( view
        , config
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
    = Config
        { toSvg : datum -> Svg Never
        , toCoordinate : datum -> ( Float, Float )
        }


config : (datum -> Svg Never) -> (datum -> ( Float, Float )) -> Config datum
config toSvg toCoordinate =
    Config { toSvg = toSvg, toCoordinate = toCoordinate }


view : Config datum -> List datum -> Html Never
view (Config { toCoordinate, toSvg }) data =
    let
        each : Float -> Float -> datum -> Svg Never
        each x_ y_ =
            Svg.circle [ cx (toString x_), cy (toString y_), r "0.5" ] << List.singleton << toSvg
    in
        data
            |> List.map
                (\datum ->
                    case toCoordinate datum of
                        ( x, y ) ->
                            each x y datum
                )
            |> Svg.svg
                [ viewBox "-6 -6 12 12"
                , width "100vw"
                , height "100vh"
                ]
