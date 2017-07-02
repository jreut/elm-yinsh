module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Board exposing (Board)
import View.Board as BoardView
import Coordinate.Hexagonal exposing (toCartesian)


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
                |> Board.add -1 -5 Black Ring
                |> Board.add -1 4 White Ring
                |> Board.add 0 -4 Black Ring
                |> Board.add 0 4 Black Disc
                |> Board.add 1 -4 Black Ring
                |> Board.add 1 2 White Ring
                |> Board.add 1 5 White Disc
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
        cartesian : ( Float, Float )
        cartesian =
            toCartesian 1 ( x, y )

        toCoordinate : Int -> String
        toCoordinate =
            toString << toFloat

        baseAttrs : List (Svg.Attribute Never)
        baseAttrs =
            let
                ( x, y ) =
                    cartesian
            in
                [ Svg.Attributes.cx (toString x)
                , Svg.Attributes.cy (toString y)
                ]

        emptyAttrs : List (Svg.Attribute Never)
        emptyAttrs =
            [ Svg.Attributes.r "1%", Svg.Attributes.fill "grey" ]

        ringAttrs : String -> List (Svg.Attribute Never)
        ringAttrs color =
            [ Svg.Attributes.r "4%"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke color
            , Svg.Attributes.strokeWidth "1%"
            ]

        discAttrs : String -> List (Svg.Attribute Never)
        discAttrs color =
            [ Svg.Attributes.r "3%", Svg.Attributes.fill color ]

        makeCircle : List (Svg.Attribute Never) -> Svg Never
        makeCircle attrs =
            Svg.circle (baseAttrs ++ attrs) []
    in
        case occupant of
            Nothing ->
                makeCircle emptyAttrs

            Just ( player, marker ) ->
                let
                    makeAttrs =
                        case marker of
                            Disc ->
                                discAttrs

                            Ring ->
                                ringAttrs
                in
                    player
                        |> toString
                        |> String.toLower
                        |> makeAttrs
                        |> makeCircle



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
