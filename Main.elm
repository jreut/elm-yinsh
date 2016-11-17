module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Coordinate =
    ( Int, Int )


type alias Position =
    { coordinate : Coordinate
    , occupant : Occupant
    }


type Occupant
    = Empty
    | Ring Player
    | Marker Player


type Player
    = White
    | Black


type alias Board =
    List Position


type alias Model =
    { board : Board }


type Msg
    = NoOp


{-| Directions in which one may traverse
-}
type Direction
    = Up
    | Down
    | Left
    | Right
    | In
    | Out


main =
    Html.program
        { init = init
        , update = (\msg model -> model ! [])
        , subscriptions = \_ -> Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    model.board
        |> List.map viewPosition
        |> Svg.svg
            [ viewBox "-10 -10 20 20"
            , height "100vh"
            , width "100vw"
            , fontSize "5%"
            ]


viewPosition : Position -> Svg Msg
viewPosition position =
    let
        x_ =
            Tuple.first position.coordinate |> toFloat |> (*) 2

        y_ =
            Tuple.second position.coordinate |> toFloat |> (*) 2

        x__ =
            (sqrt 3) / 2 * x_

        y__ =
            (x_ / 2) - y_

        xString =
            toString x__

        yString =
            toString y__
    in
        Svg.g
            []
            [ Svg.circle
                [ cx xString
                , cy yString
                , r "1%"
                , fill "none"
                , stroke "lightgrey"
                , strokeWidth "0.5%"
                ]
                []
            , Svg.text_
                [ x xString
                , y yString
                , fontStyle "italic"
                , textAnchor "middle"
                , alignmentBaseline "auto"
                ]
                [ Svg.text (toString position.coordinate) ]
            ]


radius : Float
radius =
    4.6


init : ( Model, Cmd Msg )
init =
    { board = initBoard } ! []


initBoard : Board
initBoard =
    let
        roundedRadius =
            ceiling radius

        range =
            List.range (negate roundedRadius) roundedRadius
    in
        range
            |> List.concatMap (\x -> range |> List.map (\y -> ( x, y )))
            |> List.filter valid
            |> List.map (\e -> { coordinate = e, occupant = Empty })


square : number -> number
square n =
    n * n


valid : Coordinate -> Bool
valid ( x, y ) =
    let
        x_ =
            toFloat x

        y_ =
            toFloat y
    in
        (square radius) >= (square ((sqrt 3) / 2 * x_)) + (square ((x_ / 2) - y_))


add : Direction -> Coordinate -> Coordinate
add direction ( x, y ) =
    let
        vec =
            vector direction

        dx =
            Tuple.first vec

        dy =
            Tuple.second vec
    in
        ( x + dx, y + dy )


vector : Direction -> Coordinate
vector direction =
    case direction of
        Up ->
            ( 0, 1 )

        Down ->
            ( 0, -1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )

        In ->
            ( 1, 1 )

        Out ->
            ( -1, -1 )
