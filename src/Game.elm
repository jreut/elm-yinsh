module Game
    exposing
        ( State
        , click
        , init
        , foldl
        )

import List.Extra
import Maybe.Extra
import Coordinate.Hexagonal exposing (Coordinate)
import Player exposing (Player)
import Board exposing (Board, Position)


type State
    = State
        { moves : List Move
        }


type Move
    = PlaceRing Player Coordinate


type alias Rule =
    Coordinate -> State -> Maybe Move


rules : List Rule
rules =
    [ firstMove
    , ringPlacement
    ]


firstMove : Rule
firstMove coordinate (State { moves }) =
    if List.isEmpty moves then
        Just (PlaceRing Player.white coordinate)
    else
        Nothing


ringPlacement : Rule
ringPlacement coordinate (State { moves }) =
    let
        board =
            foldl (State { moves = moves })

        predicates =
            [ List.length moves < 10
            , Board.isEmpty coordinate board
            ]
    in
        if List.all identity predicates then
            case moves of
                (PlaceRing player _) :: _ ->
                    Just (PlaceRing (Player.next player) coordinate)

                _ ->
                    Nothing
        else
            Nothing


init : State
init =
    State { moves = [] }


click : Position -> State -> State
click position state =
    let
        applyRule : Rule -> Maybe Move
        applyRule rule =
            rule (Board.coordinate position) state

        moves : List Move
        moves =
            rules
                |> List.map applyRule
                |> Maybe.Extra.values

        addMove move (State { moves }) =
            State { moves = move :: moves }
    in
        case moves of
            [] ->
                Debug.log "no rule applies" state

            move :: _ ->
                addMove move state


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
