module Game
    exposing
        ( State
        , Move
        , init
        , update
        , board
        , message
        , availableMoves
        , movesForCoordinate
          -- for testing
        , jumpCoordinate
        , freedomsForRay
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Board exposing (Board, Position)
import Board.Occupant as Occupant exposing (Occupant)
import Coordinate.Hexagonal exposing (Coordinate)
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


type MovingPhase
    = PlacingDisc
    | DroppingRing Coordinate


type Phase
    = PlacingRings
    | MovingRings MovingPhase


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

        MovingRings movingPhase ->
            case movingPhase of
                PlacingDisc ->
                    ringsFor toMove board
                        |> List.filter
                            (\coordinate ->
                                freedomsFor coordinate board
                                    |> not
                                    << Set.isEmpty
                            )
                        |> List.map (StartMovingRing toMove)

                DroppingRing origin ->
                    freedomsFor origin board
                        |> Set.toList
                        |> List.map (DropRing toMove origin)


ringCount : Board Player Marker -> Int
ringCount board =
    let
        filter coord player marker =
            Ring == marker
    in
        Board.filter filter board
            |> Dict.size


ringsFor : Player -> Board Player Marker -> List Coordinate
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


freedomsForRay : List (Position Player Marker) -> List ( Int, Int )
freedomsForRay ray =
    let
        ( empties, rest ) =
            span
                (\position -> Occupant.isEmpty position.occupant)
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
            case Occupant.toMaybe position.occupant of
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

        MovingRings movingPhase ->
            case movingPhase of
                PlacingDisc ->
                    (toString toMove) ++ " to move"

                DroppingRing _ ->
                    (toString toMove) ++ " to place their ring"



-- UPDATE


type Move
    = -- AddRing player at@(x,y)
      AddRing Player Coordinate
      -- StartMovingRing player from@(x,y)
    | StartMovingRing Player Coordinate
      -- DropRing player from@(x,y) to@(x,y)
    | DropRing Player Coordinate Coordinate


movesForCoordinate : Coordinate -> List Move -> List Move
movesForCoordinate coordinate =
    let
        filter move =
            case move of
                AddRing _ target ->
                    target == coordinate

                StartMovingRing _ from ->
                    from == coordinate

                DropRing _ _ to ->
                    to == coordinate
    in
        List.filter filter


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard move state.board }
        |> updatePhase move


updateBoard : Move -> Board Player Marker -> Board Player Marker
updateBoard move board =
    case move of
        AddRing player coordinate ->
            Board.add coordinate player Ring board

        StartMovingRing player from ->
            Board.add from player Disc board

        DropRing player from to ->
            let
                flip =
                    Occupant.update (\( player, marker ) -> ( Player.next player, marker ))
            in
                board
                    |> Board.add to player Ring
                    |> Board.updateBetween flip from to


moveRing : Player -> ( Int, Int ) -> ( Int, Int ) -> Board Player Marker -> Board Player Marker
moveRing player from to board =
    board
        |> Board.add from player Disc
        |> Board.add to player Ring


updatePhase : Move -> State -> State
updatePhase move (State state) =
    case state.phase of
        PlacingRings ->
            if ringCount state.board < 10 then
                nextPlayer (State state)
            else
                nextPlayer
                    (State
                        { state
                            | phase = MovingRings PlacingDisc
                        }
                    )

        MovingRings movingPhase ->
            case movingPhase of
                PlacingDisc ->
                    (State
                        { state
                            | phase = MovingRings (DroppingRing (droppingRingOrigin move))
                        }
                    )

                DroppingRing _ ->
                    nextPlayer
                        (State
                            { state
                                | phase = MovingRings PlacingDisc
                            }
                        )


nextPlayer : State -> State
nextPlayer (State state) =
    State { state | toMove = Player.next state.toMove }


droppingRingOrigin : Move -> Coordinate
droppingRingOrigin move =
    case move of
        StartMovingRing _ origin ->
            origin

        _ ->
            Debug.crash ("This should have been a " ++ (toString StartMovingRing))
