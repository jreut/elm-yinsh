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


type alias Msg =
    Game.Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    { model | game = Game.update msg model.game } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Yinsh" ]
        , tableOfAvailableMoves model
        ]


tableOfAvailableMoves : Model -> Html Msg
tableOfAvailableMoves { game } =
    Html.table []
        ([ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "(x, y)" ]
                , Html.th [] [ Html.text "action" ]
                , Html.th [] [ Html.text "do it" ]
                ]
            ]
         ]
            ++ List.map makeRowFromMove (Game.availableMoves game)
        )


makeRowFromMove : Game.Move -> Html Msg
makeRowFromMove move =
    Html.tr []
        [ Html.td [] [ move |> Game.coordinateFromMove |> toString |> Html.text ]
        , Html.td [] [ move |> Game.actionFromMove |> toString |> Html.text ]
        , Html.td []
            [ Html.button [ onClick move ] []
            ]
        ]
