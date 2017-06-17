module View.Board
    exposing
        ( view
        , config
        )

import Svg exposing (Svg)


type Config data
    = Config
        { toSvg : data -> Svg Never
        , toCoordinate : data -> ( Float, Float )
        }


config : (data -> Svg Never) -> (data -> ( Float, Float )) -> Config data
config toSvg toCoordinate =
    Config { toSvg = toSvg, toCoordinate = toCoordinate }


view : Config data -> List data -> Svg Never
view config data =
    Svg.text "hello, SVG"
