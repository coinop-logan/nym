module Vector2 exposing (..)

import Length
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity
import Vector2d exposing (Vector2d)


type alias Vector2 =
    { x : Float
    , y : Float
    }


zero : Vector2
zero =
    Vector2 0 0


fromLengthAndRadians : Float -> Float -> Vector2
fromLengthAndRadians length angle =
    Vector2
        (length * cos angle)
        (length * sin angle)


fromTuple : ( Float, Float ) -> Vector2
fromTuple ( x, y ) =
    Vector2 x y


fromMetersPoint : Point2d Length.Meters () -> Vector2
fromMetersPoint p =
    p |> Point2d.toRecord Length.inMeters


toMetersVector : Vector2 -> Vector2d Length.Meters ()
toMetersVector v =
    Vector2d.unsafe v


toMetersPoint : Vector2 -> Point2d Length.Meters ()
toMetersPoint v =
    Point2d.unsafe v


negate : Vector2 -> Vector2
negate =
    scaleBy -1


plus : Vector2 -> Vector2 -> Vector2
plus v1 v2 =
    Vector2
        (v1.x + v2.x)
        (v1.y + v2.y)


minus : Vector2 -> Vector2 -> Vector2
minus subtrahend minuend =
    plus (negate subtrahend) minuend


scaleBy : Float -> Vector2 -> Vector2
scaleBy scale v =
    Vector2
        (v.x * scale)
        (v.y * scale)


scaleByVector : Vector2 -> Vector2 -> Vector2
scaleByVector scaleVec v =
    Vector2
        (scaleVec.x * v.x)
        (scaleVec.y * v.y)


normalize : Vector2 -> Vector2
normalize v =
    let
        length =
            magnitude v
    in
    Vector2
        (v.x / length)
        (v.y / length)


magnitude : Vector2 -> Float
magnitude v =
    Vector2d.unsafe v
        |> Vector2d.length
        |> Quantity.unwrap


type alias Quad =
    { bottomRight : Vector2
    , bottomLeft : Vector2
    , topLeft : Vector2
    , topRight : Vector2
    }


quadToMetersPolygon : Quad -> Polygon2d Length.Meters ()
quadToMetersPolygon quad =
    [ quad.bottomRight
    , quad.bottomLeft
    , quad.topLeft
    , quad.topRight
    ]
        |> List.map toMetersPoint
        |> Polygon2d.singleLoop


interpolate : Float -> Vector2 -> Vector2 -> Vector2
interpolate f v1 v2 =
    minus v1 v2
        |> scaleBy f
        |> plus v1
