module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Board
import Player
import Coordinate exposing (Coordinate)


type Occupant
    = Empty
    | Ring Player.Model
    | Marker Player.Model


type alias Model =
    { board : Board.Model Occupant
    , currentPlayer : Player.Model
    }


type Msg
    = NoOp
    | PlaceRing Coordinate


radius : Float
radius =
    4.6


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    model.board
        |> Dict.toList
        |> List.map viewPosition
        |> Svg.svg
            [ viewBox "-10 -10 20 20"
            , height "100vh"
            , width "100vw"
            , fontSize "5%"
            ]
        |> (\svg -> Html.main_ [ Html.Attributes.style [ ( "background-color", "lightblue" ) ] ] [ svg ])


viewPosition : ( Coordinate, Occupant ) -> Svg Msg
viewPosition ( coordinate, occupant ) =
    let
        ( x_, y_ ) =
            toCartesian 2 coordinate

        ( xString, yString ) =
            toCartesian 2 coordinate
                |> Tuple.mapFirst toString
                |> Tuple.mapSecond toString
    in
        Svg.g
            [ onClick (PlaceRing coordinate) ]
            [ Svg.circle [ cx xString, cy yString, r "3%", fill "none", pointerEvents "all" ] []
            , viewLines x_ y_
            , viewOccupant occupant xString yString
            ]


viewLines : Float -> Float -> Svg Msg
viewLines x y =
    let
        line =
            \x_ y_ ->
                Svg.line
                    [ x1 (toString x)
                    , y1 (toString y)
                    , x2 (toString (x + x_))
                    , y2 (toString (y + y_))
                    , stroke "black"
                    , strokeWidth "0.3%"
                    ]
                    []
    in
        Svg.g
            []
            [ line 0 1
            , line 0 -1
            , line ((sqrt 3) / 2) (1 / 2)
            , line ((sqrt 3) / 2) (-1 / 2)
            , line ((sqrt 3) / -2) (-1 / 2)
            , line ((sqrt 3) / -2) (1 / 2)
            ]


viewCoordinate : Coordinate -> Svg Msg
viewCoordinate coordinate =
    Svg.text (toString coordinate)


viewOccupant : Occupant -> String -> String -> Svg Msg
viewOccupant occupant cx_ cy_ =
    case occupant of
        Ring player ->
            Svg.circle
                [ cx cx_, cy cy_, r "4%", fill "none", stroke (Player.view player), strokeWidth "1%" ]
                []

        Marker player ->
            Svg.circle
                [ cx cx_, cy cy_, r "2%", fill (Player.view player) ]
                []

        Empty ->
            Svg.circle
                [ cx cx_, cy cy_, r "0", fill "none" ]
                []


toCartesian : Float -> Coordinate -> ( Float, Float )
toCartesian scale coordinate =
    let
        x =
            Tuple.first coordinate |> toFloat

        y =
            Tuple.second coordinate |> toFloat
    in
        ( (sqrt 3) / 2 * x, (x / 2) - y )
            |> Tuple.mapFirst ((*) scale)
            |> Tuple.mapSecond ((*) scale)


init : ( Model, Cmd Msg )
init =
    { board = Board.init radius Empty
    , currentPlayer = Player.init
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaceRing coordinate ->
            { model
                | board = Board.update coordinate (Ring model.currentPlayer) model.board
                , currentPlayer = Player.update model.currentPlayer
            }
                ! []

        NoOp ->
            model ! []
