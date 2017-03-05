module Main exposing (main)

import Html exposing (Html)
import Svg exposing (Svg)
import Board
import View.Board
import Coordinate.Hexagonal exposing (toCartesian)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    () ! []



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ title
        , board model
        , footer
        ]


board : Model -> Html Msg
board model =
    let
        data =
            Board.toList Board.empty

        config =
            View.Board.config toCoordinate toMsg toSvg
    in
        View.Board.view config data


toCoordinate : Board.Position -> ( Float, Float )
toCoordinate =
    toCartesian 2 << Board.coordinate


toMsg : Board.Position -> Maybe Msg
toMsg =
    Just << always NoOp


toSvg : Board.Position -> Svg Msg
toSvg =
    Svg.text << toString


title : Html Msg
title =
    Html.header []
        [ Html.h1 [] [ Html.text "Yinsh" ]
        ]


footer : Html Msg
footer =
    Html.footer []
        [ Html.text "by jreut" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
