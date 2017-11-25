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


type Phase
    = PlacingRings
    | MovingRings
    | RemovingRuns
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
                |> List.map AddRing

        MovingRings ->
            ringsFor toMove board
                |> List.concatMap
                    (\from ->
                        freedomsFor from board
                            |> Set.toList
                            |> List.map (MoveRing from)
                    )

        RemovingRuns ->
            -- TODO: sort by toMove to let the moving player remove first
            runsToRemove board
                |> List.concatMap
                    (\run ->
                        List.map
                            (RemoveRun run)
                            (ringsFor (Run.player run) board)
                    )

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

                MovingRings ->
                    toString toMove ++ " to move"

                RemovingRuns ->
                    "Removal of runs"

                GameOver player ->
                    toString player ++ " has won"

        scoreMessage =
            toString Black ++ " has " ++ toString blackScore ++ ", " ++ toString White ++ " has " ++ toString whiteScore
    in
        String.join ", " [ phaseMessage, scoreMessage ]



-- UPDATE


type Move
    = -- AddRing at@(x,y)
      AddRing Coordinate
      -- MoveRing from@(x,y) to@(x,y)
    | MoveRing Coordinate Coordinate
      -- RemoveRun run ring@(x,y)
    | RemoveRun Run Coordinate


movesForCoordinate : Coordinate -> List Move -> List Move
movesForCoordinate coordinate =
    let
        filter move =
            case move of
                AddRing target ->
                    target == coordinate

                MoveRing from _ ->
                    from == coordinate

                RemoveRun run _ ->
                    Run.coordinates run |> Set.member coordinate
    in
        List.filter filter


update : Move -> State -> State
update move (State state) =
    State { state | board = updateBoard state.toMove move state.board }
        |> updateScore move
        |> updatePhase move
        |> checkScore


updateBoard : Player -> Move -> Board -> Board
updateBoard toMove move board =
    case move of
        AddRing coordinate ->
            Board.add coordinate toMove Ring board

        MoveRing from to ->
            let
                flip =
                    Occupant.update (Tuple.mapFirst Player.next)
            in
                Board.add from toMove Disc board
                    |> Board.add to toMove Ring
                    |> Board.updateBetween flip from to

        RemoveRun run ring ->
            Set.foldl Board.remove board (Run.coordinates run)
                |> Board.remove ring


moveRing : Player -> Coordinate -> Coordinate -> Board -> Board
moveRing player from to board =
    board
        |> Board.add from player Disc
        |> Board.add to player Ring


updateScore : Move -> State -> State
updateScore move (State state) =
    case move of
        RemoveRun run _ ->
            case Run.player run of
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
                    (State { state | phase = MovingRings })

        MovingRings ->
            if List.isEmpty <| runsToRemove state.board then
                nextPlayer (State { state | phase = MovingRings })
            else
                State { state | phase = RemovingRuns }

        RemovingRuns ->
            if List.isEmpty <| runsToRemove state.board then
                nextPlayer (State { state | phase = MovingRings })
            else
                State { state | phase = RemovingRuns }

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
