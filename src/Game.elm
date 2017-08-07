module Game
    exposing
        ( State
        , init
        , board
        , message
        )

import Board exposing (Board)
import Player exposing (Player(..))
import Marker exposing (Marker(..))


type State
    = State
        { toMove : Player
        , whiteScore : Int
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
            { toMove = White
            , whiteScore = 0
            , blackScore = 0
            , board = board
            }


board : State -> Board Player Marker
board (State { board }) =
    board


message : State -> String
message =
    always "Welcome to Yinsh!"
