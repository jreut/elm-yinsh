module Game
    exposing
        ( State
        , init
        , board
        , update
          -- TRASH BELOW
        , Coordinate
        , addArbitraryRing
        , phase
        , emptyPositions
        )

import Dict
import Board
import Marker exposing (Marker, disc, ring)
import Player exposing (Player)
import Occupant exposing (Occupant)


type State
    = State
        { phase : Phase
        , board : Board
        }


type alias Board =
    Board.Model (Occupant (Marker Player))


type Phase
    = InitialRingPlacement Phase
    | End


init : State
init =
    State
        { phase =
            InitialRingPlacement
                (InitialRingPlacement
                    (InitialRingPlacement
                        (InitialRingPlacement
                            (InitialRingPlacement
                                (InitialRingPlacement
                                    (InitialRingPlacement
                                        (InitialRingPlacement
                                            (InitialRingPlacement
                                                (InitialRingPlacement End)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        , board = Board.init 4.6 Occupant.init
        }


nextPhase : State -> State
nextPhase (State state) =
    case state.phase of
        InitialRingPlacement nextPhase ->
            State { state | phase = nextPhase }

        End ->
            init


board : State -> Board
board (State { board }) =
    board



-- UPDATE


update : Coordinate -> State -> State
update coordinate (State state) =
    case state.phase of
        InitialRingPlacement _ ->
            State state
                |> placeRing coordinate Player.init
                |> nextPhase

        End ->
            State state


placeRing : Coordinate -> Player -> State -> State
placeRing coordinate player (State state) =
    State { state | board = Board.insert coordinate (Occupant.from (ring player)) state.board }



-- TRASH


type alias Coordinate =
    Board.Coordinate


addArbitraryRing : State -> State
addArbitraryRing (State state) =
    let
        emptyKeys =
            state.board
                |> Dict.filter (\k v -> v == Occupant.init)
                |> Dict.keys
    in
        case List.head emptyKeys of
            Nothing ->
                State state

            Just k ->
                State { state | board = Board.insert k (ring Player.init |> Occupant.from) state.board } |> nextPhase


phase : State -> String
phase (State { phase }) =
    case phase of
        InitialRingPlacement _ ->
            "initial ring placement"

        _ ->
            "unknown"


emptyPositions : State -> List Board.Coordinate
emptyPositions (State { board }) =
    board
        |> Dict.filter (\k v -> v == Occupant.init)
        |> Dict.keys
