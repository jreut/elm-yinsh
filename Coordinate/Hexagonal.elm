module Coordinate.Hexagonal
    exposing
        ( Coordinate
        , validWithin
        , squareOf
        , toCartesian
        , toString
        )


type alias Coordinate =
    ( Int, Int )


squareOf : Int -> List Coordinate
squareOf radius =
    let
        range =
            List.range (negate radius) radius
    in
        List.concatMap (\x -> List.map (\y -> ( x, y )) range) range


validWithin : Float -> Coordinate -> Bool
validWithin radius coordinate =
    radius ^ 2 >= norm coordinate origin


norm : Coordinate -> Coordinate -> Float
norm origin dest =
    let
        ( x, y ) =
            toCartesian 1 origin

        ( x_, y_ ) =
            toCartesian 1 dest
    in
        (x - x_) ^ 2 + (y - y_) ^ 2


toCartesian : Float -> Coordinate -> ( Float, Float )
toCartesian scale coordinate =
    let
        ( x, y ) =
            toFloat coordinate

        x_ =
            (sqrt 3) / 2 * x * scale

        y_ =
            ((x / 2) - y) * scale
    in
        ( x_, y_ )


toFloat : Coordinate -> ( Float, Float )
toFloat ( x, y ) =
    ( Basics.toFloat x, Basics.toFloat y )


toString : ( number, number ) -> ( String, String )
toString ( x, y ) =
    ( Basics.toString x, Basics.toString y )


origin : Coordinate
origin =
    ( 0, 0 )



-- type Direction
--     = Up
--     | Down
--     | Left
--     | Right
--     | In
--     | Out
-- add : Direction -> Coordinate -> Coordinate
-- add direction ( x, y ) =
--     let
--         vec =
--             vector direction
--         dx =
--             Tuple.first vec
--         dy =
--             Tuple.second vec
--     in
--         ( x + dx, y + dy )
-- vector : Direction -> Coordinate
-- vector direction =
--     case direction of
--         Up ->
--             ( 0, 1 )
--         Down ->
--             ( 0, -1 )
--         Left ->
--             ( -1, 0 )
--         Right ->
--             ( 1, 0 )
--         In ->
--             ( 1, 1 )
--         Out ->
--             ( -1, -1 )
