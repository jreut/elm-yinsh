module View.Board
    exposing
        ( view
        , config
        )

import Svg exposing (Svg)


type Config datum
    = Config
        { toSvg : datum -> Svg Never
        , toCoordinate : datum -> ( Float, Float )
        }


config : (datum -> Svg Never) -> (datum -> ( Float, Float )) -> Config datum
config toSvg toCoordinate =
    Config { toSvg = toSvg, toCoordinate = toCoordinate }


view : Config datum -> List datum -> Svg Never
view config data =
    Svg.text (toString data)
