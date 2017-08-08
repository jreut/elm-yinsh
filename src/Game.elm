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
        }


init : State
init =
    let
        board =
            Board.init 4.6
                |> Board.add -1 -5 Black Ring
                |> Board.add -1 4 White Ring
                |> Board.add 0 -4 Black Ring
                |> Board.add 0 4 Black Disc
                |> Board.add 1 -4 Black Ring
                |> Board.add 1 2 White Ring
                |> Board.add 1 5 White Disc
                |> Board.add 2 0 Black Disc
    in
        State
            { whiteScore = 0
            , blackScore = 0
            , board = board
            }


board : State -> Board Player Marker
board (State { board }) =
    board



-- VIEW


availableMoves : State -> List Move
availableMoves (State { board }) =
    Board.emptyPositions board |> List.map (AddRing White)


message : State -> String
message =
    always "Welcome to Yinsh!"



-- UPDATE


type Move
    = AddRing Player ( Int, Int )


update : Move -> State -> State
update move (State state) =
    case move of
        AddRing player ( x, y ) ->
            State { state | board = Board.add x y player Ring state.board }
