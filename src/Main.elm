module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
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
    { game : Game.State
    }


init : ( Model, Cmd Msg )
init =
    { game = Game.init } ! []



-- UPDATE


type Msg
    = NoOp
    | ClickedCoordinate Game.Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ClickedCoordinate coordinate ->
            { model | game = Game.update coordinate model.game } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Yinsh" ]
        , Html.h2 [] [ Html.text (Game.phase model.game) ]
        , Html.div []
            (model.game
                |> Game.emptyPositions
                |> List.map
                    (\x ->
                        Html.button
                            [ onClick (ClickedCoordinate x) ]
                            [ x |> toString |> Html.text ]
                    )
            )
        ]
