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
        |> (\svg -> Html.body [ Html.Attributes.style [ ( "background-color", "lightblue" ) ] ] [ svg ])


viewPosition : Position -> Svg Msg
viewPosition { coordinate, occupant } =
    let
        ( x_, y_ ) =
            toCartesian 2 coordinate

        ( xString, yString ) =
            toCartesian 2 coordinate
                |> Tuple.mapFirst toString
                |> Tuple.mapSecond toString
    in
        Svg.g
            []
            [ viewLines x_ y_
            , viewOccupant occupant xString yString
            ]


viewLines : Float -> Float -> Svg Msg
viewLines x y =
    let
        line =
            \x_ y_ ->
                Svg.line
                    [ x1 (toString x)
                    , y1 (toString y)
                    , x2 (toString (x + x_))
                    , y2 (toString (y + y_))
                    , stroke "black"
                    , strokeWidth "0.3%"
                    ]
                    []
    in
        Svg.g
            []
            [ line 0 1
            , line 0 -1
            , line ((sqrt 3) / 2) (1 / 2)
            , line ((sqrt 3) / 2) (-1 / 2)
            , line ((sqrt 3) / -2) (-1 / 2)
            , line ((sqrt 3) / -2) (1 / 2)
            ]


viewCoordinate : Coordinate -> Svg Msg
viewCoordinate coordinate =
    Svg.text (toString coordinate)


playerColor : Player -> String
playerColor player =
    case player of
        White ->
            "white"

        Black ->
            "black"


viewOccupant : Occupant -> String -> String -> Svg Msg
viewOccupant occupant cx_ cy_ =
    case occupant of
        Ring player ->
            Svg.circle
                [ cx cx_, cy cy_, r "4%", fill "none", stroke (playerColor player), strokeWidth "1%" ]
                []

        Marker player ->
            Svg.circle
                [ cx cx_, cy cy_, r "2%", fill (playerColor player) ]
                []

        Empty ->
            Svg.circle
                [ cx cx_, cy cy_, r "0", fill "none" ]
                []


toCartesian : Float -> Coordinate -> ( Float, Float )
toCartesian scale coordinate =
    let
        x =
            Tuple.first coordinate |> toFloat

        y =
            Tuple.second coordinate |> toFloat
    in
        ( (sqrt 3) / 2 * x, (x / 2) - y )
            |> Tuple.mapFirst ((*) scale)
            |> Tuple.mapSecond ((*) scale)


radius : Float
radius =
    4.6


init : ( Model, Cmd Msg )
init =
    { board = initBoard } ! []


initBoard : Board
initBoard =
    squareOf (ceiling radius)
        |> List.filter valid
        |> List.map (\e -> { coordinate = e, occupant = Empty })


squareOf : Int -> List Coordinate
squareOf radius =
    let
        range =
            List.range (negate radius) radius
    in
        List.concatMap (\x -> List.map (\y -> ( x, y )) range) range


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
