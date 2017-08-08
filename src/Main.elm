module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Board exposing (Board)
import Game
import View.Board as BoardView
import View.Occupant as OccupantView


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
            BoardView.toSvgAndMsg
                (Svg.g [] << List.singleton << OccupantView.view)
                (always NoOp)

        boardView =
            BoardView.view config (game |> Game.board |> Board.toList)
    in
        Html.main_
            [ style
                [ ( "backgroundColor"
                  , "lightblue"
                  )
                ]
            ]
            [ boardView
            , messagesView (Game.message game)
            , actionsView (Game.availableMoves game)
            ]


messagesView : String -> Html Msg
messagesView message =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "100vw" )
            , ( "height", "10vh" )
            ]
        ]
        [ Html.text message ]


actionsView : List Game.Move -> Html Msg
actionsView moves =
    let
        makeButton =
            \move ->
                Html.button
                    [ onClick (MakeMove move) ]
                    [ Html.text (toString move) ]
    in
        Html.div [] (List.map makeButton moves)



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
