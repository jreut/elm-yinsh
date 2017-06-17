module Coordinate.Hexagonal
    exposing
        ( Coordinate
        , maybeValid
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
        List.concatMap (\x -> List.map ((,) x) range) range


maybeValid : Float -> Coordinate -> Maybe Coordinate
maybeValid radius coordinate =
    if validWithin radius coordinate then
        Just coordinate
    else
        Nothing


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
            (sqrt 3) / 2 * x

        y_ =
            ((x / 2) - y)
    in
        ( x_ * scale, y_ * scale )


toFloat : Coordinate -> ( Float, Float )
toFloat ( x, y ) =
    ( Basics.toFloat x, Basics.toFloat y )


toString : ( number, number ) -> ( String, String )
toString ( x, y ) =
    ( Basics.toString x, Basics.toString y )


origin : Coordinate
origin =
    ( 0, 0 )
