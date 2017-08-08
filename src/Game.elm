module Game
    exposing
        ( State
        , Move
        , init
        , update
        , board
        , message
        , availableMoves
        )

import Board exposing (Board)
import Player exposing (Player(..))
import Marker exposing (Marker(..))


-- MODEL


type State
    = State
        { whiteScore : Int
        , blackScore : Int
        , board : Board Player Marker
        , toMove : Player
        }


init : State
init =
    State
        { whiteScore = 0
        , blackScore = 0
        , board = Board.init 4.6
        , toMove = White
        }


board : State -> Board Player Marker
board (State { board }) =
    board



-- VIEW


availableMoves : State -> List Move
availableMoves (State { board, toMove }) =
    Board.emptyPositions board |> List.map (AddRing toMove)


message : State -> String
message (State { toMove }) =
    (toString toMove) ++ " to place a ring"



-- UPDATE


type Move
    = AddRing Player ( Int, Int )


update : Move -> State -> State
update move (State state) =
    case move of
        AddRing player ( x, y ) ->
            State
                { state
                    | board = Board.add x y player Ring state.board
                    , toMove = Player.next state.toMove
                }
