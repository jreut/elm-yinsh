module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Board exposing (Board)
import View.Board as BoardView
import View.Occupant as OccupantView
import Coordinate.Hexagonal exposing (toCartesian)
import Player exposing (Player(..))
import Marker exposing (Marker(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    let
        board =
            Board.init 4.6
                |> Board.add -1 -5 Black Ring
                |> Board.add -1 4 White Ring
                |> Board.add 0 -4 Black Ring
                |> Board.add 0 4 Black Disc
                |> Board.add 1 -4 Black Ring
                |> Board.add 1 2 White Ring
                |> Board.add 1 5 White Disc
                |> Board.add 2 0 Black Disc
    in
        { board = board } ! []



--- MODEL


type alias Model =
    { board : Board Player Marker }



--- VIEW


view : Model -> Html Msg
view { board } =
    let
        config =
            BoardView.toSvg (Svg.g [] << List.singleton << OccupantView.view)
    in
        BoardView.view config (Board.view board)
            |> Html.map (always NoOp)
            |> List.singleton
            |> Html.main_
                [ Html.Attributes.style
                    [ ( "backgroundColor"
                      , "lightblue"
                      )
                    ]
                ]



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



--- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
