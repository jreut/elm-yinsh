module Main exposing (..)

import Set exposing (Set)
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
      -- TODO: implement removal
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
    = PlaceRing Hex.Coordinate
    | PlaceMarker Hex.Coordinate
    | MoveRing Hex.Coordinate Hex.Coordinate
    | RemoveRun Hex.Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updater =
            case msg of
                PlaceRing coordinate ->
                    placeRing coordinate

                PlaceMarker coordinate ->
                    placeMarker coordinate

                MoveRing from to ->
                    moveRing from to

                RemoveRun coordinate ->
                    removeRun coordinate
    in
        updater model ! []


placeRing : Hex.Coordinate -> Model -> Model
placeRing coordinate model =
    let
        placeRing_ =
            \player -> Dict.insert coordinate (Ring player) model.board
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
                | board = Dict.insert coordinate (Marker player) model.board
                , phase = MovingRing coordinate player
            }

        _ ->
            model


moveRing : Hex.Coordinate -> Hex.Coordinate -> Model -> Model
moveRing from to model =
    let
        flipped =
            Board.line from to model.board
                |> List.foldr flipMarker model.board

        nextPhase player =
            if (runsOfFive model.board) |> Debug.log "five" |> List.isEmpty then
                PlacingMarker (Player.update player)
            else
                RemovingRun player

        -- RemovingRun player
    in
        case model.phase of
            MovingRing _ player ->
                { model
                    | board = Dict.insert to (Ring player) flipped
                    , phase = nextPhase player
                }

            _ ->
                model


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


removeRun : Hex.Coordinate -> Model -> Model
removeRun coordinate model =
    case Dict.get coordinate model.board of
        Just (Marker player) ->
            { model
                | board = Dict.insert coordinate Empty model.board
                , phase = PlacingMarker (Player.update player)
            }

        _ ->
            model



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
                    markerPlacement model player

                MovingRing coordinate _ ->
                    ringReplacement model coordinate

                RemovingRun player ->
                    runRemoval model player

                _ ->
                    initialRingPlacement
    in
        { toCoordinate = toCoordinate
        , toMsg = \_ -> toMsg
        , toSvg = toSvg
        }


toCoordinate : Position -> Board.View.Coordinate
toCoordinate =
    Tuple.first >> Hex.toCartesian 2


initialRingPlacement : Position -> Maybe Msg
initialRingPlacement position =
    case position of
        ( coordinate, Empty ) ->
            Just (PlaceRing coordinate)

        _ ->
            Nothing


markerPlacement : Model -> Player -> Position -> Maybe Msg
markerPlacement model player position =
    if moveableRing model position player then
        Tuple.first position |> PlaceMarker |> Just
    else
        Nothing


moveableRing : Model -> Position -> Player -> Bool
moveableRing model ( coordinate, occupant ) player =
    let
        playersEqual other =
            other == player

        hasMoves =
            availableMoves coordinate model.board |> not << Set.isEmpty
    in
        case occupant of
            Ring player_ ->
                playersEqual player_ && hasMoves

            _ ->
                False


ringReplacement : Model -> Hex.Coordinate -> Position -> Maybe Msg
ringReplacement model origin ( destination, _ ) =
    if validMove model origin destination then
        MoveRing origin destination |> Just
    else
        Nothing


runRemoval : Model -> Player -> Position -> Maybe Msg
runRemoval model player ( coordinate, occupant ) =
    let
        marked =
            markerRuns model.board
                |> List.map Set.fromList
                |> List.foldl Set.diff Set.empty
    in
        case occupant of
            Marker player_ ->
                if player_ == player then
                    Just (RemoveRun coordinate)
                else
                    Nothing

            _ ->
                Nothing


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
    availableMoves origin model.board
        |> Set.member destination


availableMoves : Hex.Coordinate -> Board -> Set Hex.Coordinate
availableMoves origin board =
    Board.filterRays jumpCoordinates origin board
        |> List.concatMap identity
        |> Set.fromList



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


markers : Board -> Board
markers =
    let
        filter _ v =
            case v of
                Marker _ ->
                    True

                _ ->
                    False
    in
        Dict.filter filter


markerRuns : Board -> List (List Hex.Coordinate)
markerRuns board =
    markers board
        |> Dict.foldl (\k _ -> ((::) (Board.contiguousLines k board))) []
        |> List.concatMap identity


runsOfFive : Board -> List (List Hex.Coordinate)
runsOfFive =
    markerRuns
        >> List.filter (List.length >> ((==) 4))
