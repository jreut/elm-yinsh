module Coordinate
    exposing
        ( Coordinate
        , validWithin
        , squareOf
        , toCartesian
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
validWithin radius ( x, y ) =
    let
        x_ =
            toFloat x

        y_ =
            toFloat y
    in
        (radius ^ 2) >= (((sqrt 3) / 2 * x_) ^ 2) + (((x_ / 2) - y_) ^ 2)


toCartesian : Float -> Coordinate -> ( Float, Float )
toCartesian scale coordinate =
    let
        ( x, y ) =
            coordinate |> Tuple.mapFirst toFloat |> Tuple.mapSecond toFloat
    in
        ( (sqrt 3) / 2 * x, (x / 2) - y )
            |> Tuple.mapFirst ((*) scale)
            |> Tuple.mapSecond ((*) scale)



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
