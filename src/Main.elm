module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Board exposing (Board)
import Game
import View.Board as BoardView
import View.Occupant as OccupantView
import Player exposing (Player)
import Marker exposing (Marker)


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
        config =
            BoardView.config
                { toSvg = toSvg game
                , toMsg = toMsg game
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
            , messagesView (Game.message game)
            ]


toSvg : Game.State -> Board.Position Player Marker -> Svg Msg
toSvg game position =
    let
        shouldHighlight =
            Game.movesForCoordinate position.coordinate (Game.availableMoves game)
                |> not
                << List.isEmpty
    in
        OccupantView.view shouldHighlight position


toMsg : Game.State -> Board.Position Player Marker -> Msg
toMsg game { coordinate } =
    Game.movesForCoordinate coordinate (Game.availableMoves game)
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
        , Html.h3 [] [ Html.text "v0.0.2" ]
        ]


messagesView : String -> Html Msg
messagesView message =
    Html.div
        [ Html.Attributes.style
            []
        ]
        [ Html.p [] [ Html.text message ]
        , Html.p [] [ Html.em [] [ Html.text "Click on any highlighted position to move." ] ]
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
