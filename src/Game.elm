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

import Dict
import Board
import Marker exposing (Marker, disc, ring)
import Player exposing (Player)
import Occupant exposing (Occupant)


type State
    = State
        { actions : List (Action Player)
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
        { actions = []
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
    case move of
        ( coord, PlaceRing player ) ->
            State { state | board = placeRing coord player state.board }

        _ ->
            State state


placeRing : Coordinate -> Player -> Board -> Board
placeRing coordinate player board =
    Board.insert coordinate (Occupant.from (disc player)) board


availableMoves : State -> List Move
availableMoves (State { board, actions }) =
    case List.head actions of
        Nothing ->
            initialRingPlacement Player.init board

        Just action ->
            case action of
                PlaceRing player ->
                    initialRingPlacement (Player.next player) board

                _ ->
                    []


initialRingPlacement : Player -> Board -> List Move
initialRingPlacement player board =
    emptyCoordinates board |> List.map (\x -> Move ( x, PlaceRing player ))


emptyCoordinates : Board -> List Coordinate
emptyCoordinates =
    Dict.keys << Dict.filter (always ((==) Occupant.init))


actionFromMove (Move ( _, action )) =
    action


coordinateFromMove (Move ( coordinate, _ )) =
    coordinate
