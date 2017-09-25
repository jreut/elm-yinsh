module Game
    exposing
        ( State
        , Move
        , init
        , update
        , board
        , message
        , availableMoves
          -- for testing
        , jumpCoordinate
        , freedomsForRay
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Board exposing (Board, Position)
import Board.Occupant exposing (Occupant)
import Player exposing (Player(..))
import Marker exposing (Marker(..))
import List.Extra exposing (span)


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


availableMoves : State -> List Move
availableMoves (State { board, toMove, phase }) =
    case phase of
        PlacingRings ->
            Board.emptyPositions board
                |> Set.toList
                |> List.map (AddRing toMove)

        MovingRings ->
            ringsFor toMove board
                |> List.concatMap
                    (\coordinate ->
                        freedomsFor coordinate board
                            |> Set.toList
                            |> List.map (MoveRing toMove coordinate)
                    )


ringCount : Board Player Marker -> Int
ringCount board =
    let
        filter coord player marker =
            Ring == marker
    in
        Board.filter filter board
            |> Dict.size


ringsFor : Player -> Board Player Marker -> List ( Int, Int )
ringsFor player board =
    let
        filter coord player_ marker =
            player == player_ && Ring == marker
    in
        Board.filter filter board |> Board.coordinates


freedomsFor : ( Int, Int ) -> Board Player Marker -> Set ( Int, Int )
freedomsFor coordinate board =
    Board.raysFrom coordinate board
        |> List.concatMap freedomsForRay
        |> Set.fromList
        |> Debug.log ("freedoms for " ++ (toString coordinate))


freedomsForRay : List (Position Player Marker) -> List ( Int, Int )
freedomsForRay ray =
    let
        ( empties, rest ) =
            span
                (\position -> Board.Occupant.isEmpty position.occupant)
                ray

        positions =
            case jumpCoordinate rest of
                Nothing ->
                    empties

                Just position ->
                    empties ++ [ position ]
    in
        List.map (.coordinate) positions


jumpCoordinate : List (Position Player Marker) -> Maybe (Position Player Marker)
jumpCoordinate xs =
    case xs of
        [] ->
            Nothing

        position :: ys ->
            case Board.Occupant.toMaybe position.occupant of
                Just ( player, marker ) ->
                    case marker of
                        Ring ->
                            Nothing

                        Disc ->
                            jumpCoordinate ys

                _ ->
                    Just position



-- VIEW


message : State -> String
message (State { toMove, board, phase }) =
    case phase of
        PlacingRings ->
            (toString toMove) ++ " to place a ring (" ++ (ringCount board |> toString) ++ " rings placed)"

        MovingRings ->
            (toString toMove) ++ " to move"



-- UPDATE


type Move
    = -- AddRing player at@(x,y)
      AddRing Player ( Int, Int )
      -- MoveRing player from@(x,y) to@(x,y)
    | MoveRing Player ( Int, Int ) ( Int, Int )


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard move state.board } |> updatePhase


updateBoard : Move -> Board Player Marker -> Board Player Marker
updateBoard move board =
    case move of
        AddRing player coordinate ->
            Board.add coordinate player Ring board

        MoveRing player from to ->
            moveRing player from to board


moveRing : Player -> ( Int, Int ) -> ( Int, Int ) -> Board Player Marker -> Board Player Marker
moveRing player from to board =
    board
        |> Board.add from player Disc
        |> Board.add to player Ring


updatePhase : State -> State
updatePhase (State state) =
    case state.phase of
        PlacingRings ->
            if ringCount state.board < 10 then
                nextPlayer (State state)
            else
                nextPlayer
                    (State
                        { state
                            | phase = MovingRings
                        }
                    )

        MovingRings ->
            nextPlayer (State state)


nextPlayer : State -> State
nextPlayer (State state) =
    State { state | toMove = Player.next state.toMove }
