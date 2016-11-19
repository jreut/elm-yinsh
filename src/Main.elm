module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , fill
        , stroke
        , strokeWidth
        )
import Svg.Events exposing (onClick)
import Board
import Player exposing (Player)
import Coordinate.Hexagonal as Hex
import BoardView


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { board : Board
    , currentPlayer : Player
    , boardView : BoardView.State
    }


type alias Board =
    Board.Model Occupant


type alias Position =
    Board.Position Occupant


type Occupant
    = Empty
    | Ring Player
    | Marker Player


init : ( Model, Cmd Msg )
init =
    { board = Board.init radius Empty
    , currentPlayer = Player.init
    , boardView = BoardView.init
    }
        ! []


radius : Float
radius =
    4.6



-- UPDATE


type Msg
    = NoOp
    | PlaceRing Hex.Coordinate
    | PlaceMarker Hex.Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaceRing coordinate ->
            placeOccupant coordinate (Ring model.currentPlayer) model ! []

        PlaceMarker coordinate ->
            placeOccupant coordinate (Marker model.currentPlayer) model ! []

        NoOp ->
            model ! []


placeOccupant : Hex.Coordinate -> Occupant -> Model -> Model
placeOccupant coordinate occupant model =
    { model
        | board = Board.update coordinate occupant model.board
        , currentPlayer = Player.update model.currentPlayer
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        svg =
            Board.positions model.board
                |> BoardView.view boardConfig ()
    in
        Html.main_
            [ style [ ( "background-color", "lightblue" ) ] ]
            [ svg ]


boardConfig : BoardView.Config Position Msg
boardConfig =
    { toCoordinate = toCoordinate
    , toMsg = toMsg
    , toSvg = toSvg
    }


toCoordinate : Position -> BoardView.Coordinate
toCoordinate ( coordinate, _ ) =
    Hex.toCartesian 2 coordinate


toMsg : BoardView.State -> Position -> Msg
toMsg _ position =
    case position of
        ( coordinate, Empty ) ->
            PlaceRing coordinate

        ( coordinate, Ring _ ) ->
            NoOp

        ( _, Marker _ ) ->
            NoOp


toSvg : Position -> Svg Msg
toSvg ( coordinate, occupant ) =
    let
        circle_ =
            case occupant of
                Ring player ->
                    circle "4%" "none" [ stroke (Player.view player), strokeWidth "1%" ]

                Marker player ->
                    circle "2%" (Player.view player) []

                Empty ->
                    circle "0" "none" []
    in
        circle_ coordinate


circle : String -> String -> List (Svg.Attribute Msg) -> Hex.Coordinate -> Svg Msg
circle radius fill_ attrs coordinate =
    let
        ( x, y ) =
            coordinate |> Hex.toCartesian 2 |> Hex.toString
    in
        Svg.circle ([ cx x, cy y, r radius, fill fill_ ] ++ attrs) []
