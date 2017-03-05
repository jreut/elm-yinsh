module Game
    exposing
        ( State
        , click
        , init
        , foldl
        )

import Coordinate.Hexagonal exposing (Coordinate)
import Player exposing (Player)
import Board exposing (Board, Position)


type State
    = State
        { moves : List Move
        }


type Move
    = PlaceRing Player Coordinate


init : State
init =
    State { moves = [] }


click : Position -> State -> State
click position (State { moves }) =
    case ( position, moves ) of
        ( position, [] ) ->
            State { moves = (PlaceRing Player.white (Board.coordinate position)) :: moves }

        _ ->
            Debug.crash "not yet handled"


foldl : State -> Board
foldl (State { moves }) =
    let
        foldMove : Move -> Board -> Board
        foldMove move =
            case move of
                PlaceRing player coordinate ->
                    Board.ringAt coordinate player
    in
        List.foldl foldMove Board.empty moves
