module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Board exposing (Board)
import Game
import View.Board as BoardView
import View.Occupant as OccupantView
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
        { board = board
        , state = Game.init |> Game.nextPlayer |> Game.nextPlayer
        }
            ! []



--- MODEL


type alias Model =
    { board : Board Player Marker
    , state : Game.State
    }



--- VIEW


view : Model -> Html Msg
view model =
    let
        { board, state } =
            model

        config =
            BoardView.toSvgAndMsg
                (Svg.g [] << List.singleton << OccupantView.view)
                (always NoOp)

        boardView =
            BoardView.view config (Board.view board)
    in
        Html.main_
            [ Html.Attributes.style
                [ ( "backgroundColor"
                  , "lightblue"
                  )
                ]
            ]
            [ boardView, messagesView model ]


messagesView : Model -> Html Msg
messagesView { state } =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "100vw" )
            , ( "height", "20vh" )
            ]
        ]
        [ Html.text ((Game.toMove state |> toString) ++ " to move") ]



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
