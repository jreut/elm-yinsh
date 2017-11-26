module Main exposing (main)

import Board exposing (Board)
import Game
import Game.Run as Run exposing (Run)
import Html exposing (Html)
import Html.Attributes exposing (href, style)
import Marker exposing (Marker)
import Player exposing (Player)
import Svg exposing (Svg)
import View.Board as BoardView
import View.Occupant as OccupantView
import View.Score as ScoreView
import Coordinate.Hexagonal exposing (Coordinate)


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
    { game = Game.init
    , phase = Normal
    }
        ! []



--- MODEL


type Phase
    = Normal
      -- MovingRing from@(x,y)
    | MovingRing Coordinate
    | RemovingRun Run


type alias Model =
    { game : Game.State
    , phase : Phase
    }



--- VIEW


view : Model -> Html Msg
view { game, phase } =
    let
        availableMoves =
            Game.availableMoves game

        config =
            BoardView.config
                { toSvg = toSvg phase availableMoves
                , toMsg = toMsg phase availableMoves
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


toSvg : Phase -> List Game.Move -> Board.Position Player Marker -> Svg Msg
toSvg phase availableMoves position =
    let
        moves =
            case phase of
                Normal ->
                    availableMoves
                        |> Game.movesForCoordinate position.coordinate

                MovingRing from ->
                    availableMoves
                        |> List.filter (Game.matchesMoveRing from position.coordinate)

                RemovingRun run ->
                    availableMoves
                        |> List.filter (Game.matchesRemoveRun run position.coordinate)

        shouldHighlight =
            moves |> not << List.isEmpty
    in
        OccupantView.view shouldHighlight position


toMsg : Phase -> List Game.Move -> Board.Position Player Marker -> Msg
toMsg phase availableMoves { coordinate } =
    case phase of
        Normal ->
            Game.movesForCoordinate coordinate availableMoves
                |> List.map
                    (\move ->
                        if Game.isMoveRing move then
                            StartMovingRing coordinate
                        else if Game.isRemoveRun move then
                            StartRemovingRun (Game.getRunToRemove move)
                        else
                            MakeMove move
                    )
                |> List.head
                |> Maybe.withDefault NoOp

        MovingRing from ->
            availableMoves
                |> List.filterMap
                    (\move ->
                        if Game.matchesMoveRing from coordinate move then
                            Just <| DropRing coordinate
                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault NoOp

        RemovingRun run ->
            availableMoves
                |> List.filterMap
                    (\move ->
                        if Game.matchesRemoveRun run coordinate move then
                            Just <| ChooseRing coordinate
                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault NoOp


header : Html Never
header =
    Html.header
        [ style [ ( "flex-basis", "20%" ) ]
        ]
        [ Html.h1 [] [ Html.text "Yinsh" ]
        , Html.h2 [] [ Html.text "on the Web!" ]
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
            [ ( "flex-basis", "20%" ) ]
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
      -- StartMovingRing from@(x,y)
    | StartMovingRing Coordinate
    | DropRing Coordinate
    | StartRemovingRun Run
    | ChooseRing Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        MakeMove move ->
            { model | game = Game.update move model.game } ! []

        StartMovingRing from ->
            { model | phase = MovingRing from } ! []

        DropRing to ->
            case model.phase of
                MovingRing from ->
                    let
                        move =
                            Game.mkMoveRing from to
                    in
                        { model
                            | phase = Normal
                            , game = Game.update move model.game
                        }
                            ! []

                _ ->
                    Debug.crash "should have been in MovingRing phase"

        StartRemovingRun run ->
            { model | phase = RemovingRun run } ! []

        ChooseRing coordinate ->
            case model.phase of
                RemovingRun run ->
                    let
                        move =
                            Game.mkRemoveRun run coordinate
                    in
                        { model
                            | phase = Normal
                            , game = Game.update move model.game
                        }
                            ! []

                _ ->
                    Debug.crash "should have been in RemovingRun phase"
