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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



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
        ]
