module Main exposing (main)

import Html exposing (Html)


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
    ()


init : ( Model, Cmd Msg )
init =
    () ! []



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ [] [ Html.text "hello" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
