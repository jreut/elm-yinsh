module Main exposing (..)

import Set
import Dict
import List.Extra
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
import Board
import Player exposing (Player)
import Coordinate.Hexagonal as Hex
import Board.View


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
    , boardView : Board.View.State
    , phase : Phase Player
    }


type alias Board =
    Board.Model Occupant


type alias Position =
    Board.Position Occupant


type Occupant
    = Empty
    | Ring Player
    | Marker Player


type Phase a
    = PlacingRing Int a
    | PlacingMarker a
    | MovingRing Hex.Coordinate a
    | RemovingRing a
    | RemovingRun a


init : ( Model, Cmd Msg )
init =
    { board = Board.init radius Empty
    , boardView = Board.View.init
    , phase = PlacingRing 9 Player.init
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
    | MoveRing Hex.Coordinate Hex.Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        model_ =
            case msg of
                PlaceRing coordinate ->
                    placeRing coordinate model

                PlaceMarker coordinate ->
                    placeMarker coordinate model

                MoveRing from to ->
                    moveRing from to model

                NoOp ->
                    model
    in
        model_ ! []


placeRing : Hex.Coordinate -> Model -> Model
placeRing coordinate model =
    let
        placeRing_ =
            \player -> Board.update coordinate (Ring player) model.board
    in
        case model.phase of
            PlacingRing 0 player ->
                { model
                    | board = placeRing_ player
                    , phase = PlacingMarker (Player.update player)
                }

            PlacingRing remaining player ->
                { model
                    | board = placeRing_ player
                    , phase = PlacingRing (remaining - 1) (Player.update player)
                }

            _ ->
                model


placeMarker : Hex.Coordinate -> Model -> Model
placeMarker coordinate model =
    case model.phase of
        PlacingMarker player ->
            { model
                | board = Board.update coordinate (Marker player) model.board
                , phase = MovingRing coordinate player
            }

        _ ->
            model


moveRing : Hex.Coordinate -> Hex.Coordinate -> Model -> Model
moveRing from to model =
    let
        flipped =
            Board.line from to model.board
                |> flipMarkers model.board
    in
        case model.phase of
            MovingRing _ player ->
                { model
                    | board = Board.update to (Ring player) flipped
                    , phase = PlacingMarker (Player.update player)
                }

            _ ->
                model


flipMarkers : Board -> List Hex.Coordinate -> Board
flipMarkers =
    List.foldr flipMarker


flipMarker : Hex.Coordinate -> Board -> Board
flipMarker coordinate =
    let
        flip occupant =
            case occupant of
                Marker player ->
                    Marker (Player.update player)

                _ ->
                    occupant
    in
        Dict.update coordinate (Maybe.map flip)



-- VIEW


view : Model -> Html Msg
view model =
    let
        svg =
            Board.positions model.board
                |> Board.View.view (boardConfig model) model.boardView
    in
        Html.main_
            [ style [ ( "background-color", "lightblue" ) ] ]
            [ svg ]


boardConfig : Model -> Board.View.Config Position Msg
boardConfig model =
    let
        toMsg =
            case model.phase of
                PlacingRing _ _ ->
                    initialRingPlacement

                PlacingMarker player ->
                    markerPlacement player

                MovingRing coordinate _ ->
                    ringReplacement coordinate model

                _ ->
                    initialRingPlacement
    in
        { toCoordinate = toCoordinate
        , toMsg = \_ -> toMsg
        , toSvg = toSvg
        , disabledMsg = NoOp
        }


toCoordinate : Position -> Board.View.Coordinate
toCoordinate ( coordinate, _ ) =
    Hex.toCartesian 2 coordinate


initialRingPlacement : Position -> Msg
initialRingPlacement position =
    case position of
        ( coordinate, Empty ) ->
            PlaceRing coordinate

        _ ->
            NoOp


markerPlacement : Player -> Position -> Msg
markerPlacement player position =
    case position of
        ( coordinate, Ring player_ ) ->
            if player_ == player then
                PlaceMarker coordinate
            else
                NoOp

        _ ->
            NoOp


ringReplacement : Hex.Coordinate -> Model -> Position -> Msg
ringReplacement origin model ( destination, _ ) =
    if validMove model origin destination then
        MoveRing origin destination
    else
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



-- FUN STUFF


validMove : Model -> Hex.Coordinate -> Hex.Coordinate -> Bool
validMove model origin destination =
    Board.filteredRuns jumpCoordinates origin model.board
        |> Set.member destination



-- All of this totally copied from @sharkdp
-- https://github.com/sharkdp/yinsh/blob/master/src/Yinsh.hs#L228-L237


jumpCoordinates : Board.RunFilter Occupant
jumpCoordinates model xs =
    let
        ( free, rest ) =
            List.Extra.span (isEmpty model) xs
    in
        case jumpCoordinate model rest of
            Nothing ->
                free

            Just a ->
                free ++ [ a ]


isEmpty : Board -> Hex.Coordinate -> Bool
isEmpty model coord =
    case Dict.get coord model of
        Nothing ->
            False

        Just occupant ->
            case occupant of
                Empty ->
                    True

                _ ->
                    False


jumpCoordinate : Board -> List Hex.Coordinate -> Maybe Hex.Coordinate
jumpCoordinate model xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            case Dict.get y model of
                Nothing ->
                    Nothing

                Just (Ring _) ->
                    Nothing

                Just (Marker _) ->
                    jumpCoordinate model ys

                Just Empty ->
                    Just y
