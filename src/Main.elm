module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Board exposing (Board)
import View.Board as BoardView
import Svg exposing (Svg)
import Svg.Attributes


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
        config =
            BoardView.toSvg (Svg.g [] << List.singleton << viewOccupant)
    in
        BoardView.view config (Board.view board)
            |> Html.map (always NoOp)
            |> List.singleton
            |> Html.main_
                [ Html.Attributes.style
                    [ ( "backgroundColor"
                      , "lightblue"
                      )
                    ]
                ]


viewOccupant : ( Int, Int, Maybe ( Player, Marker ) ) -> Svg Never
viewOccupant ( x, y, occupant ) =
    let
        toCoordinate : Int -> String
        toCoordinate =
            toString << toFloat

        baseAttrs : List (Svg.Attribute Never)
        baseAttrs =
            [ Svg.Attributes.cx (toCoordinate x)
            , Svg.Attributes.cy (toCoordinate y)
            ]

        emptyAttrs : List (Svg.Attribute Never)
        emptyAttrs =
            [ Svg.Attributes.r "0.1", Svg.Attributes.fill "grey" ]

        ringAttrs : String -> List (Svg.Attribute Never)
        ringAttrs color =
            [ Svg.Attributes.r "0.5"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke color
            , Svg.Attributes.strokeWidth "0.15"
            ]

        discAttrs : String -> List (Svg.Attribute Never)
        discAttrs color =
            [ Svg.Attributes.r "0.45", Svg.Attributes.fill color ]
    in
        case occupant of
            Nothing ->
                Svg.circle (baseAttrs ++ emptyAttrs) []

            Just ( player, marker ) ->
                let
                    color =
                        player |> toString |> String.toLower
                in
                    case marker of
                        Disc ->
                            Svg.circle (baseAttrs ++ (discAttrs color)) []

                        Ring ->
                            Svg.circle (baseAttrs ++ (ringAttrs color)) []



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
