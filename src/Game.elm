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

import Dict exposing (Dict)
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
        , phase : Phase
        }


type Phase
    = PlacingRings
    | MovingRings


init : State
init =
    State
        { whiteScore = 0
        , blackScore = 0
        , board = Board.init 4.6
        , toMove = White
        , phase = PlacingRings
        }


board : State -> Board Player Marker
board (State { board }) =
    board



-- VIEW


availableMoves : State -> List Move
availableMoves (State { board, toMove, phase }) =
    case phase of
        PlacingRings ->
            Board.emptyPositions board |> List.map (AddRing toMove)

        MovingRings ->
            -- WIP
            []


ringCount : Board Player Marker -> Int
ringCount board =
    let
        filter coord player marker =
            Ring == marker
    in
        Board.filter filter board
            |> Dict.size


message : State -> String
message (State { toMove, board, phase }) =
    case phase of
        PlacingRings ->
            (toString toMove) ++ " to place a ring (" ++ (ringCount board |> toString) ++ " rings placed)"



-- UPDATE


type Move
    = AddRing Player ( Int, Int )
    | MoveRing Player ( Int, Int ) ( Int, Int )


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard move state.board } |> updatePhase


updateBoard : Move -> Board Player Marker -> Board Player Marker
updateBoard move board =
    case move of
        AddRing player ( x, y ) ->
            Board.add x y player Ring board

        MoveRing player from to ->
            moveRing player from to board


moveRing : Player -> ( Int, Int ) -> ( Int, Int ) -> Board Player Marker -> Board Player Marker
moveRing player ( fromX, fromY ) ( toX, toY ) board =
    board
        |> Board.add fromX fromY player Disc
        |> Board.add toX toY player Ring


updatePhase : State -> State
updatePhase (State state) =
    case state.phase of
        PlacingRings ->
            nextPlayer (State state)


nextPlayer : State -> State
nextPlayer (State state) =
    State { state | toMove = Player.next state.toMove }
