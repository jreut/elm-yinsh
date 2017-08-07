module Game
    exposing
        ( State
        , init
        , toMove
        , nextPlayer
        )

import Dict exposing (Dict)
import Player exposing (Player(..))


type State
    = State
        { toMove : Player
        , whiteScore : Int
        , blackScore : Int
        }


init : State
init =
    State
        { toMove = White
        , whiteScore = 0
        , blackScore = 0
        }


toMove : State -> Player
toMove (State { toMove }) =
    toMove


nextPlayer : State -> State
nextPlayer (State state) =
    State { state | toMove = (Player.next state.toMove) }


incrementScore : Player -> State -> State
incrementScore player (State state) =
    case player of
        White ->
            State { state | whiteScore = state.whiteScore + 1 }

        Black ->
            State { state | blackScore = state.blackScore + 1 }
