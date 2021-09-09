module Vector3 exposing (..)

import Length
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


type alias RectBounds =
    ( Vector3, Vector3 )


type alias Vector3 =
    { x : Float
    , y : Float
    , z : Float
    }


zero : Vector3
zero =
    Vector3 0 0 0


toMetersVector : Vector3 -> Vector3d Length.Meters ()
toMetersVector v =
    Vector3d.unsafe v


toMetersPoint : Vector3 -> Point3d Length.Meters ()
toMetersPoint v =
    Point3d.unsafe v


negate : Vector3 -> Vector3
negate =
    scaleBy -1


plus : Vector3 -> Vector3 -> Vector3
plus v1 v2 =
    Vector3
        (v1.x + v2.x)
        (v1.y + v2.y)
        (v1.z + v2.z)


minus : Vector3 -> Vector3 -> Vector3
minus subtrahend minuend =
    plus (negate subtrahend) minuend


scaleBy : Float -> Vector3 -> Vector3
scaleBy scale v =
    Vector3
        (v.x * scale)
        (v.y * scale)
        (v.z * scale)


scaleByVector : Vector3 -> Vector3 -> Vector3
scaleByVector scaleVec v =
    Vector3
        (scaleVec.x * v.x)
        (scaleVec.y * v.y)
        (scaleVec.z * v.z)


normalize : Vector3 -> Vector3
normalize v =
    let
        length =
            magnitude v
    in
    Vector3
        (v.x / length)
        (v.y / length)
        (v.z / length)


magnitude : Vector3 -> Float
magnitude v =
    Vector3d.unsafe v
        |> Vector3d.length
        |> Quantity.unwrap
