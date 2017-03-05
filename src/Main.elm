module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Board
import View.Board
import View.Marker
import Coordinate.Hexagonal exposing (toCartesian)
import Game


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
    { game : Game.State }


init : ( Model, Cmd Msg )
init =
    { game = Game.init } ! []



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.style [ ( "background-color", "lightblue" ) ]
        ]
        [ title
        , board model
        , footer
        ]


board : Model -> Html Msg
board model =
    let
        data =
            Board.toList <| Game.foldl model.game

        config =
            View.Board.config toCoordinate toMsg toSvg
    in
        View.Board.view config data


toCoordinate : Board.Position -> ( Float, Float )
toCoordinate =
    toCartesian 2 << Board.coordinate


toMsg : Board.Position -> Maybe Msg
toMsg =
    Just << Clicked


toSvg : Board.Position -> Svg Msg
toSvg position =
    View.Marker.toSvg (Board.coordinate position) (Board.occupant position)
        |> Svg.map never


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
    | Clicked Board.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            model ! []

        Clicked position ->
            { model | game = Game.click position model.game } ! []
