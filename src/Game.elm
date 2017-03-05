module Game
    exposing
        ( State
        , Move
        , init
        , update
        , availableMoves
        , actionFromMove
        , coordinateFromMove
        )

import List.Extra
import Dict
import Board
import Marker exposing (Marker, disc, ring, isRing, isDisc)
import Player exposing (Player)
import Occupant exposing (Occupant)


type State
    = State
        { moves : List Move
        , board : Board
        }


type Action player
    = RemoveRing
    | RemoveRun (List Coordinate) player
    | PlaceMarker player
    | PlaceRing player
    | MoveRing Coordinate


type alias Board =
    Board.Model (Occupant (Marker Player))


type Phase
    = InitialRingPlacement Phase
    | End


init : State
init =
    State
        { moves = []
        , board = Board.init 4.6 Occupant.init
        }


board : State -> Board
board (State { board }) =
    board


type alias Coordinate =
    Board.Coordinate


type Move
    = Move ( Coordinate, Action Player )


update : Move -> State -> State
update (Move move) (State state) =
    let
        nextBoard =
            case move of
                ( coord, PlaceRing player ) ->
                    placeRing coord player

                _ ->
                    identity

        addMove =
            (::) (Move move)
    in
        State
            { state
                | board = nextBoard state.board
                , moves = addMove state.moves
            }


placeRing : Coordinate -> Player -> Board -> Board
placeRing coordinate player board =
    Board.insert coordinate (Occupant.lift (ring player)) board


availableMoves : State -> List Move
availableMoves (State { board, moves }) =
    case List.head moves of
        Nothing ->
            initialRingPlacement Player.init board

        Just (Move ( coordinate, action )) ->
            case action of
                PlaceRing player ->
                    if ringCount board >= 10 then
                        markerPlacement (Player.next player) board
                    else
                        initialRingPlacement (Player.next player) board

                PlaceMarker player ->
                    ringMovement player coordinate board

                _ ->
                    []


initialRingPlacement : Player -> Board -> List Move
initialRingPlacement player board =
    emptyCoordinates board |> List.map (\x -> Move ( x, PlaceRing player ))


markerPlacement : Player -> Board -> List Move
markerPlacement player board =
    let
        ringsForPlayer : Player -> Board.Coordinate -> Occupant (Marker Player) -> Bool
        ringsForPlayer =
            (\player k v -> v == Occupant.lift (ring player))
    in
        Dict.filter (ringsForPlayer player) board
            |> Dict.keys
            |> List.map (\x -> Move ( x, PlaceMarker player ))


ringMovement : Player -> Coordinate -> Board -> List Move
ringMovement player origin board =
    Board.filterRays jumpCoordinates origin board
        |> List.concatMap identity
        |> Set.fromList
        |> Set.toList


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

                Just Nothing ->
                    Just y


emptyCoordinates : Board -> List Coordinate
emptyCoordinates =
    Dict.keys << Dict.filter (always ((==) Occupant.init))


ringCount : Board -> Int
ringCount board =
    Dict.filter (\k o -> Occupant.fold o |> Maybe.map (isRing) |> Maybe.withDefault False) board |> Dict.size


actionFromMove (Move ( _, action )) =
    action


coordinateFromMove (Move ( coordinate, _ )) =
    coordinate
