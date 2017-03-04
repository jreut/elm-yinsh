module Game
    exposing
        ( State
        , next
        , init
        , board
        , addArbitraryRing
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


next : State -> State
next (State state) =
    case state.phase of
        InitialRingPlacement nextPhase ->
            State { state | phase = nextPhase }

        End ->
            init


board : State -> Board
board (State { board }) =
    board



-- TRASH


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
                State { state | board = Board.insert k (ring Player.init |> Occupant.from) state.board } |> next
