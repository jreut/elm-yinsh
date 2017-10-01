module Game
    exposing
        ( Move
        , State
        , availableMoves
        , board
        , freedomsForRay
        , init
        , jumpCoordinate
        , message
        , movesForCoordinate
          -- for testing
        , runOfFive
        , scores
        , update
        )

import Board
import Board.Occupant as Occupant exposing (Occupant)
import Coordinate.Hexagonal exposing (Coordinate)
import Dict exposing (Dict)
import Game.Run as Run exposing (Run)
import List.Extra exposing (span)
import Marker exposing (Marker(..))
import Player exposing (Player(..))
import Set exposing (Set)


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
      -- GameOver winner
    | GameOver Player


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
                    runsToRemove board |> List.map RemoveRun

                RemovingRing player ->
                    ringsFor player board |> List.map (RemoveRing player)

        GameOver _ ->
            []


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


scores : State -> { black : Int, white : Int }
scores (State state) =
    { black = state.blackScore, white = state.whiteScore }


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
    List.map .coordinate positions


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
message (State { toMove, board, phase, whiteScore, blackScore }) =
    let
        phaseMessage =
            case phase of
                PlacingRings ->
                    toString toMove ++ " to place a ring (" ++ (ringCount board |> toString) ++ " rings placed)"

                MovingRings movingPhase ->
                    case movingPhase of
                        PlacingDisc ->
                            toString toMove ++ " to move"

                        DroppingRing _ ->
                            toString toMove ++ " to place their ring"

                RemovingRuns removalPhase ->
                    case removalPhase of
                        RemovingRun ->
                            "Removal of runs"

                        RemovingRing player ->
                            toString player ++ " to remove a ring"

                GameOver player ->
                    toString player ++ " has won"

        scoreMessage =
            toString Black ++ " has " ++ toString blackScore ++ ", " ++ toString White ++ " has " ++ toString whiteScore
    in
    String.join ", " [ phaseMessage, scoreMessage ]



-- UPDATE


type Move
    = -- AddRing player at@(x,y)
      AddRing Player Coordinate
      -- StartMovingRing player from@(x,y)
    | StartMovingRing Player Coordinate
      -- DropRing player from@(x,y) to@(x,y)
    | DropRing Player Coordinate Coordinate
    | RemoveRun Run
    | RemoveRing Player Coordinate


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

                RemoveRun run ->
                    Run.coordinates run |> Set.member coordinate

                RemoveRing _ target ->
                    target == coordinate
    in
    List.filter filter


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard move state.board }
        |> updateScore move
        |> updatePhase move
        |> checkScore


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

        RemoveRun run ->
            Set.foldl Board.remove board (Run.coordinates run)

        RemoveRing _ coordinate ->
            Board.remove coordinate board


moveRing : Player -> Coordinate -> Coordinate -> Board -> Board
moveRing player from to board =
    board
        |> Board.add from player Disc
        |> Board.add to player Ring


updateScore : Move -> State -> State
updateScore move (State state) =
    case move of
        RemoveRing player _ ->
            case player of
                White ->
                    State { state | whiteScore = state.whiteScore + 1 }

                Black ->
                    State { state | blackScore = state.blackScore + 1 }

        _ ->
            State state


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
                    State { state | phase = MovingRings (DroppingRing (droppingRingOrigin move)) }

                DroppingRing _ ->
                    if List.isEmpty <| runsToRemove state.board then
                        nextPlayer (State { state | phase = MovingRings PlacingDisc })
                    else
                        State { state | phase = RemovingRuns RemovingRun }

        RemovingRuns removalPhase ->
            case removalPhase of
                RemovingRun ->
                    State { state | phase = RemovingRuns (RemovingRing (removingRunPlayer move)) }

                RemovingRing _ ->
                    if List.isEmpty <| runsToRemove state.board then
                        nextPlayer (State { state | phase = MovingRings PlacingDisc })
                    else
                        State { state | phase = RemovingRuns RemovingRun }

        GameOver player ->
            -- unreachable, but whatever
            State state


checkScore : State -> State
checkScore (State state) =
    case ( state.blackScore, state.whiteScore ) of
        ( 3, _ ) ->
            State { state | phase = GameOver Black }

        ( _, 3 ) ->
            State { state | phase = GameOver White }

        _ ->
            State state


nextPlayer : State -> State
nextPlayer (State state) =
    State { state | toMove = Player.next state.toMove }


droppingRingOrigin : Move -> Coordinate
droppingRingOrigin move =
    case move of
        StartMovingRing _ origin ->
            origin

        _ ->
            Debug.crash ("This should have been a " ++ toString StartMovingRing)


removingRunPlayer : Move -> Player
removingRunPlayer move =
    case move of
        RemoveRun run ->
            Run.player run

        _ ->
            Debug.crash ("This should have been a " ++ toString RemoveRun)


runsToRemove : Board -> List Run
runsToRemove board =
    board
        |> discs
        |> List.concatMap
            (\coordinate ->
                Board.raysWithOriginFrom coordinate board
                    |> List.filterMap runOfFive
            )
        |> Run.unique


runOfFive : List Position -> Maybe Run
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
                            |> Maybe.map (Set.fromList << List.map .coordinate)
                            |> Maybe.map (Run.init player)
                    )
