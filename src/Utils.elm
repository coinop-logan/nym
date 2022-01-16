module Utils exposing (..)

import Result.Extra
import Color exposing (Color)
import Direction3d
import Html.Attributes exposing (wrap)
import Length
import List.Extra
import Point3d exposing (Point3d)
import Quantity
import TupleHelpers
import Types exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)
import Vector3d exposing (Vector3d)


colorToRgbVector3 : Color -> Vector3
colorToRgbVector3 color =
    let
        rgba =
            color
                |> Color.toRgba
    in
    Vector3
        rgba.red
        rgba.green
        rgba.blue


unsafeRgbVector3ToColor : Vector3 -> Color
unsafeRgbVector3ToColor v =
    Color.fromRgba
        { red = v.x
        , green = v.y
        , blue = v.z
        , alpha = 1
        }


rgbVector3ToColorAndCorrectComponents : (Float -> Float) -> Vector3 -> Color
rgbVector3ToColorAndCorrectComponents correctFunc v =
    { v
        | x = v.x |> correctFunc
        , y = v.y |> correctFunc
        , z = v.z |> correctFunc
    }
        |> unsafeRgbVector3ToColor


rgbVector3ToColorAndWrap : Vector3 -> Color
rgbVector3ToColorAndWrap =
    rgbVector3ToColorAndCorrectComponents wrapColorComponent


rgbVector3ToColorAndCap : Vector3 -> Color
rgbVector3ToColorAndCap =
    rgbVector3ToColorAndCorrectComponents capColorComponent


capColorComponent : Float -> Float
capColorComponent =
    max 0 >> min 1


wrapColorComponent : Float -> Float
wrapColorComponent f =
    if f < 0 then
        f
            + 1
            |> wrapColorComponent

    else if f > 1 then
        f
            - 1
            |> wrapColorComponent

    else
        f


addVectorToColorAndWrap : Vector3 -> Color -> Color
addVectorToColorAndWrap vector color =
    Vector3.plus
        (colorToRgbVector3 color)
        vector
        |> rgbVector3ToColorAndWrap


scaleColorAndCap : Float -> Color -> Color
scaleColorAndCap scale color =
    color
        |> colorToRgbVector3
        |> Vector3.scaleBy scale
        |> rgbVector3ToColorAndCap


interpolateColors : Float -> Color -> Color -> Color
interpolateColors interp c1 c2 =
    let
        ( c1Rgba, c2Rgba ) =
            ( c1, c2 )
                |> TupleHelpers.mapTuple2 Color.toRgba
    in
    Color.fromRgba
        { red = interpolate interp c1Rgba.red c2Rgba.red
        , green = interpolate interp c1Rgba.green c2Rgba.green
        , blue = interpolate interp c1Rgba.blue c2Rgba.blue
        , alpha = 1
        }


interpolate : Float -> Float -> Float -> Float
interpolate ratio a b =
    a + (ratio * (b - a))


point3dMToVector3dM : Point3d units coordinates -> Vector3d units coordinates
point3dMToVector3dM point =
    Vector3d.from Point3d.origin point


scaleByVector : Vector3d units1 coordinates -> Vector3d units2 coordinates -> Vector3d units1 coordinates
scaleByVector scaleVec vec =
    let
        ( v1, v2 ) =
            ( Vector3d.unwrap scaleVec
            , Vector3d.unwrap vec
            )
    in
    Vector3d.unsafe
        { x = v1.x * v2.x
        , y = v1.y * v2.y
        , z = v1.z * v2.z
        }


mirrorPoint : Vector3 -> Vector3
mirrorPoint v =
    { v
        | x = -v.x
    }


interpolateVector2InQuad : Vector2.Quad -> Vector2 -> Vector2
interpolateVector2InQuad quad v =
    let
        ( yRangeStart, yRangeEnd ) =
            ( ( quad.bottomLeft, quad.bottomRight )
            , ( quad.topLeft, quad.topRight )
            )
                |> TupleHelpers.mapTuple2
                    -- interpolate with xRel to get range of Y values
                    (\( p1, p2 ) ->
                        v.x * (p2.y - p1.y) + p1.y
                    )

        ( xRangeStart, xRangeEnd ) =
            ( ( quad.bottomLeft, quad.topLeft )
            , ( quad.bottomRight, quad.topRight )
            )
                |> TupleHelpers.mapTuple2
                    -- interpolate with xRel to get range of Y values
                    (\( p1, p2 ) ->
                        v.y * (p2.x - p1.x) + p1.x
                    )
    in
    Vector2
        (v.x * (xRangeEnd - xRangeStart) + xRangeStart)
        (v.y * (yRangeEnd - yRangeStart) + yRangeStart)



-- filters out NotYetSet points, and returns either a full list of points or the first other error it encounters.


allSetStructurePoints : StructureTemplate -> Result GenError (List Vector3)
allSetStructurePoints structureTemplate =
    let
        toMirror =
            [ structureTemplate.eyeQuadInfo |> Result.map (.eyeQuad >> .topLeft)
            , structureTemplate.eyeQuadInfo |> Result.map (.eyeQuad >> .topRight)
            , structureTemplate.eyeQuadInfo |> Result.map (.eyeQuad >> .bottomLeft)
            , structureTemplate.eyeQuadInfo |> Result.map (.eyeQuad >> .bottomRight)
            , structureTemplate.noseTop
            , structureTemplate.noseBridge
            , structureTemplate.noseBottom
            , structureTemplate.cheekbone
            , structureTemplate.crownFront
            , structureTemplate.faceSideTop
            , structureTemplate.faceSideMid
            , structureTemplate.faceSideBottom
            , structureTemplate.jawPoint
            , structureTemplate.chin
            , structureTemplate.crownBack
            , structureTemplate.earAttachFrontTop
            , structureTemplate.earAttachFrontBottom
            , structureTemplate.earAttachBack
            , structureTemplate.earTip
            , structureTemplate.earAttachInside
            ]
    in
    List.append
        toMirror
        (toMirror
            |> List.map (Result.map mirrorPoint)
        )
        |> List.filter
            -- filter out all Err NotYetSet
            (\res ->
                case res of
                    Err NotYetSet ->
                        False

                    _ ->
                        True
            )
        |> Result.Extra.combine


genErrorToString : GenError -> String
genErrorToString err =
    (case err of
        NotEnoughSource ->
            "Ran out of data while generating nym structure and color."

        NotYetSet ->
            "A data field was left empty in the template."

        UnexpectedNothing info ->
            "Unexpected \"Nothing\": " ++ info ++ " | "
    )
        ++ " Check the console for more debug information."


getBoundingBox : StructureTemplate -> Result GenError Vector3.RectBounds
getBoundingBox template =
    let
        folder : Vector3 -> Maybe Vector3.RectBounds -> Maybe Vector3.RectBounds
        folder point maybeBounds =
            case maybeBounds of
                Nothing ->
                    Just ( point, point )

                Just ( boundStart, boundEnd ) ->
                    Just
                        ( Vector3
                            (min point.x boundStart.x)
                            (min point.y boundStart.y)
                            (min point.z boundStart.z)
                        , Vector3
                            (max point.x boundEnd.x)
                            (max point.y boundEnd.y)
                            (max point.z boundEnd.z)
                        )
    in
    allSetStructurePoints template
        |> Result.map
            (List.foldl folder Nothing)
        |> Result.map (Result.fromMaybe (UnexpectedNothing "getBoundingBox had no points to iterate over"))
        |> Result.Extra.join
