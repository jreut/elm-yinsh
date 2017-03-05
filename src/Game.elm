module Game
    exposing
        ( State
        , init
        , availableMoves
        , Move
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


availableMoves : State -> List Move
availableMoves _ =
    [ Move ( ( 0, 1 ), PlaceRing Player.init ) ]


actionFromMove (Move ( _, action )) =
    action


coordinateFromMove (Move ( coordinate, _ )) =
    coordinate
