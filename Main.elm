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
import BoardView


type alias Position =
    ( Coordinate, Occupant )


toCoordinate : Position -> BoardView.Coordinate
toCoordinate ( coordinate, occupant ) =
    let
        ( x, y ) =
            coordinate |> Tuple.mapFirst toFloat |> Tuple.mapSecond toFloat

        scale =
            2
    in
        ( (sqrt 3) / 2 * x, (x / 2) - y )
            |> Tuple.mapFirst ((*) scale)
            |> Tuple.mapSecond ((*) scale)


toMsg : BoardView.State -> Position -> Msg
toMsg state ( coordinate, occupant ) =
    case ( coordinate, occupant ) of
        ( c, Empty ) ->
            PlaceRing coordinate

        ( _, _ ) ->
            NoOp


toSvg : Position -> Svg Msg
toSvg position =
    let
        ( x, y ) =
            toCoordinate position
                |> Tuple.mapFirst toString
                |> Tuple.mapSecond toString

        occupant =
            Tuple.second position

        circle =
            \r_ attrs ->
                Svg.circle
                    ([ cx x, cy y, r r_ ] ++ attrs)
                    []
    in
        case occupant of
            Ring player ->
                circle "4%"
                    [ fill "none", stroke (Player.view player), strokeWidth "1%" ]

            Marker player ->
                circle "2%"
                    [ fill (Player.view player) ]

            Empty ->
                circle "0"
                    [ fill "none" ]


boardConfig : BoardView.Config Position Msg
boardConfig =
    { toCoordinate = toCoordinate
    , toMsg = toMsg
    , toSvg = toSvg
    }


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
    let
        data =
            model.board |> Dict.toList

        svg =
            BoardView.view boardConfig () data
    in
        Html.main_ [ Html.Attributes.style [ ( "background-color", "lightblue" ) ] ] [ svg ]


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
