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
        , runOfFive
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Board
import Board.Occupant as Occupant exposing (Occupant)
import Coordinate.Hexagonal exposing (Coordinate)
import Player exposing (Player(..))
import Marker exposing (Marker(..))
import List.Extra exposing (span)


type alias Board =
    Board.Board Player Marker


type alias Position =
    Board.Position Player Marker



-- MODEL


type State
    = State
        { whiteScore : Int
        , blackScore : Int
        , board : Board
        , toMove : Player
        , phase : Phase
        }


type MovingPhase
    = PlacingDisc
    | DroppingRing Coordinate


type RemovalPhase
    = RemovingRun
    | RemovingRing Player


type Phase
    = PlacingRings
    | MovingRings MovingPhase
    | RemovingRuns RemovalPhase


init : State
init =
    State
        { whiteScore = 0
        , blackScore = 0
        , board = Board.init 4.6
        , toMove = White
        , phase = PlacingRings
        }


board : State -> Board
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

        RemovingRuns removalPhase ->
            case removalPhase of
                RemovingRun ->
                    runsToRemove board
                        |> List.map (uncurry RemoveRun)

                RemovingRing player ->
                    ringsFor player board |> List.map RemoveRing


ringCount : Board -> Int
ringCount board =
    let
        filter coord player marker =
            Ring == marker
    in
        Board.filter filter board
            |> Dict.size


ringsFor : Player -> Board -> List Coordinate
ringsFor player board =
    let
        filter coord player_ marker =
            player == player_ && Ring == marker
    in
        Board.filter filter board |> Board.coordinates


discs : Board -> List Coordinate
discs board =
    board
        |> Board.filter (\coord player marker -> Disc == marker)
        |> Board.coordinates


freedomsFor : Coordinate -> Board -> Set Coordinate
freedomsFor coordinate board =
    Board.raysFrom coordinate board
        |> List.concatMap freedomsForRay
        |> Set.fromList


freedomsForRay : List Position -> List Coordinate
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


jumpCoordinate : List Position -> Maybe Position
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

        RemovingRuns removalPhase ->
            case removalPhase of
                RemovingRun ->
                    "Removal of runs"

                RemovingRing player ->
                    (toString player) ++ " to remove a ring"



-- UPDATE


type Move
    = -- AddRing player at@(x,y)
      AddRing Player Coordinate
      -- StartMovingRing player from@(x,y)
    | StartMovingRing Player Coordinate
      -- DropRing player from@(x,y) to@(x,y)
    | DropRing Player Coordinate Coordinate
    | RemoveRun Player (Set Coordinate)
    | RemoveRing Coordinate


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

                RemoveRun _ _ ->
                    False

                RemoveRing target ->
                    target == coordinate
    in
        List.filter filter


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard move state.board }
        |> updatePhase move


updateBoard : Move -> Board -> Board
updateBoard move board =
    case move of
        AddRing player coordinate ->
            Board.add coordinate player Ring board

        StartMovingRing player from ->
            Board.add from player Disc board

        DropRing player from to ->
            let
                flip =
                    Occupant.update (Tuple.mapFirst Player.next)
            in
                board
                    |> Board.add to player Ring
                    |> Board.updateBetween flip from to

        RemoveRun _ coordinates ->
            Set.foldl Board.remove board coordinates

        RemoveRing coordinate ->
            Board.remove coordinate board


moveRing : Player -> Coordinate -> Coordinate -> Board -> Board
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
                    (State { state | phase = MovingRings PlacingDisc })

        MovingRings movingPhase ->
            case movingPhase of
                PlacingDisc ->
                    (State { state | phase = MovingRings (DroppingRing (droppingRingOrigin move)) })

                DroppingRing _ ->
                    if List.isEmpty <| runsToRemove state.board then
                        nextPlayer (State { state | phase = MovingRings PlacingDisc })
                    else
                        (State { state | phase = RemovingRuns RemovingRun })

        RemovingRuns removalPhase ->
            case removalPhase of
                RemovingRun ->
                    (State { state | phase = RemovingRuns (RemovingRing (removingRunPlayer move)) })

                RemovingRing _ ->
                    if List.isEmpty <| runsToRemove state.board then
                        nextPlayer (State { state | phase = MovingRings PlacingDisc })
                    else
                        (State { state | phase = RemovingRuns RemovingRun })


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


removingRunPlayer : Move -> Player
removingRunPlayer move =
    case move of
        RemoveRun player _ ->
            player

        _ ->
            Debug.crash ("This should have been a " ++ (toString RemoveRun))


runsToRemove : Board -> List ( Player, Set Coordinate )
runsToRemove board =
    board
        |> discs
        |> List.concatMap
            (\coordinate ->
                Board.raysWithOriginFrom coordinate board
                    |> List.filterMap runOfFive
            )


runOfFive : List Position -> Maybe ( Player, Set Coordinate )
runOfFive positions =
    let
        rec occupant acc rest =
            if List.length acc == 5 then
                Just acc
            else
                case rest of
                    position :: positions ->
                        if position.occupant == occupant then
                            rec occupant (position :: acc) positions
                        else
                            Nothing

                    [] ->
                        Nothing
    in
        case positions of
            [] ->
                Nothing

            position :: rest ->
                Occupant.toMaybe position.occupant
                    |> Maybe.andThen
                        (\( player, _ ) ->
                            rec position.occupant [ position ] rest
                                |> Maybe.map (Set.fromList << List.map (.coordinate))
                                |> Maybe.map ((,) player)
                        )
