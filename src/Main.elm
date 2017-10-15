module Main exposing (main)

import Board exposing (Board)
import Game
import Html exposing (Html)
import Html.Attributes exposing (href, style)
import Marker exposing (Marker)
import Player exposing (Player)
import Svg exposing (Svg)
import View.Board as BoardView
import View.Occupant as OccupantView
import View.Score as ScoreView


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
    { game = Game.init } ! []



--- MODEL


type alias Model =
    { game : Game.State }



--- VIEW


view : Model -> Html Msg
view { game } =
    let
        availableMoves =
            Game.availableMoves game

        config =
            BoardView.config
                { toSvg = toSvg availableMoves
                , toMsg = toMsg availableMoves
                }

        boardView =
            BoardView.view config (game |> Game.board |> Board.positions)
    in
        Html.main_
            [ style
                [ ( "backgroundColor", "lightblue" )
                , ( "display", "flex" )
                , ( "flex-direction", "row" )
                , ( "width", "100%" )
                , ( "width", "100vw" )
                , ( "height", "100%" )
                , ( "height", "100vh" )
                ]
            ]
            [ Html.map (always NoOp) header
            , boardView
            , messagesView game
            ]


toSvg : List Game.Move -> Board.Position Player Marker -> Svg Msg
toSvg availableMoves position =
    let
        shouldHighlight =
            Game.movesForCoordinate position.coordinate availableMoves
                |> not
                << List.isEmpty
    in
        OccupantView.view shouldHighlight position


toMsg : List Game.Move -> Board.Position Player Marker -> Msg
toMsg availableMoves { coordinate } =
    Game.movesForCoordinate coordinate availableMoves
        |> List.head
        |> Maybe.map MakeMove
        |> Maybe.withDefault NoOp


header : Html Never
header =
    Html.header
        [ style [ ( "flex-basis", "20%" ) ]
        ]
        [ Html.h1 [] [ Html.text "Yinsh" ]
        , Html.h2 [] [ Html.text "on the Web!" ]
        , Html.h3 [] [ Html.text "v0.0.3" ]
        , Html.p []
            [ Html.a [ href "//github.com/jreut/elm-yinsh" ]
                [ Html.text "source code"
                ]
            ]
        ]


messagesView : Game.State -> Html Msg
messagesView game =
    Html.div
        [ Html.Attributes.style
            []
        ]
        [ Html.p [] [ Html.text (Game.message game) ]
        , Html.p [] [ Html.em [] [ Html.text "Click on any highlighted position to move." ] ]
        , Html.p []
            [ Html.a [ href "http://www.gipf.com/yinsh/" ] [ Html.text "Learn more" ]
            , Html.text " about the game."
            ]
        , ScoreView.view (Game.scores game)
        ]



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



--- UPDATE


type Msg
    = NoOp
    | MakeMove Game.Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        MakeMove move ->
            { model | game = Game.update move model.game } ! []
