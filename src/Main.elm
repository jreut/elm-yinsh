module Main exposing (main)

import Html exposing (Html)
import Board exposing (Board)


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
                |> Board.add 1 2 White Ring
                |> Board.add 2 0 Black Disc
    in
        { board = board } ! []



--- MODEL


type Player
    = Black
    | White


type Marker
    = Ring
    | Disc


type alias Model =
    { board : Board Player Marker }



--- VIEW


view : Model -> Html Msg
view { board } =
    let
        fromTuple ( x, y, maybeOccupant ) =
            Html.div []
                [ Html.strong [] [ Html.text (toString ( x, y )) ]
                , Html.text (toString maybeOccupant)
                ]
    in
        Board.view board
            |> List.map fromTuple
            |> Html.div []



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
