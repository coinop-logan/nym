module Nym exposing
    ( Nym, renderNym, renderNymTemplate, EyeQuadAndPupil2d, Pupil2d, EyeQuad2d, binarySourceToNymTemplateResult
    , binarySourceToNymTemplate
    )

{-| Blarg.

@docs Nym, renderNym, renderNymTemplate, EyeQuadAndPupil2d, Pupil2d, EyeQuad2d, binarySourceToNymTemplateResult, binarySourceToNymTemplate

-}

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Generate
import Html exposing (Html)
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import Pixels
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Result.Extra
import Scene3d
import Scene3d.Material as Material
import Scene3dHelpers exposing (..)
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


{-| A Physical structure and coloring of a particular Nym.
-}
type alias Nym =
    Types.Nym


{-| Given a `Nym`, render an Entity.
-}
renderNym : Nym -> Scene3d.Entity ()
renderNym =
    nymToOkTemplate
        >> renderNymTemplate False


{-| Given a `NymTemplate`, render an Entity
-}
renderNymTemplate : Bool -> NymTemplate -> Scene3d.Entity ()
renderNymTemplate showDebugLines nymTemplate =
    let
        maybeDebugLines =
            if showDebugLines then
                Scene3d.group
                    [ Scene3d.lineSegment
                        (Material.color Color.black)
                        (LineSegment3d.fromEndpoints
                            ( Vector3 0 0 0 |> Vector3.toMetersPoint
                            , Vector3 0 0 2 |> Vector3.toMetersPoint
                            )
                        )
                    ]

            else
                Scene3d.nothing

        allFeatures =
            Scene3d.group <|
                [ middleGroup
                , symmetryGroup
                , copiedSymmetryGroup
                , maybeDebugLines
                ]

        middleGroup : Scene3d.Entity ()
        middleGroup =
            Scene3d.group
                [ snoutTop
                , noseTip
                , bridge
                , forehead
                , mouth
                , chinBottom
                , neck
                , crown
                ]

        symmetryGroup =
            Scene3d.group
                [ eyeQuadAndPupil
                , snoutSideTopMajor
                , snoutSideTopMinor
                , snoutSideMiddle
                , snoutSideBottom
                , aboveCheekbone
                , aboveEye
                , belowEar
                , faceSideTop
                , jawSide
                , faceSideBottom
                , testEntity
                , crownSide
                , ear
                ]

        copiedSymmetryGroup =
            symmetryGroup
                |> mirrorGroup

        eyeQuadAndPupil : Scene3d.Entity ()
        eyeQuadAndPupil =
            Result.map2
                (\eyeQuadInfo eyeQuadColor ->
                    Scene3d.group
                        [ meterQuad
                            eyeQuadColor
                            eyeQuadInfo.eyeQuad.bottomRight
                            eyeQuadInfo.eyeQuad.bottomLeft
                            eyeQuadInfo.eyeQuad.topLeft
                            eyeQuadInfo.eyeQuad.topRight
                        , eyeQuadInfo.pupil
                            |> List.map
                                (\triangle ->
                                    meterTriangle
                                        Color.black
                                        (triangle |> TupleHelpers.tuple3First)
                                        (triangle |> TupleHelpers.tuple3Middle)
                                        (triangle |> TupleHelpers.tuple3Last)
                                )
                            |> Scene3d.group
                        ]
                )
                nymTemplate.structure.eyeQuadInfo
                nymTemplate.coloring.eyeQuad
                |> defaultAndLogEntityError "eyeQuadAndPupil"

        eyeQuadResult =
            nymTemplate.structure.eyeQuadInfo |> Result.map .eyeQuad

        snoutTop : Scene3d.Entity ()
        snoutTop =
            meterQuadWithDefaults
                "snoutTop"
                nymTemplate.coloring.snoutTop
                nymTemplate.structure.noseTop
                nymTemplate.structure.noseBridge
                (nymTemplate.structure.noseBridge |> Result.map mirrorPoint)
                (nymTemplate.structure.noseTop |> Result.map mirrorPoint)

        snoutSideTopMajor : Scene3d.Entity ()
        snoutSideTopMajor =
            meterQuadWithDefaults
                "snoutSideTopMajor"
                nymTemplate.coloring.snoutSideTopMajor
                (eyeQuadResult |> Result.map .bottomLeft)
                nymTemplate.structure.noseTop
                nymTemplate.structure.cheekbone
                (eyeQuadResult |> Result.map .bottomRight)

        snoutSideTopMinor : Scene3d.Entity ()
        snoutSideTopMinor =
            Scene3d.group
                [ meterTriangleWithDefaults
                    "snoutSideTopMinor"
                    nymTemplate.coloring.snoutSideTopMinor
                    nymTemplate.structure.noseTop
                    nymTemplate.structure.noseBridge
                    (eyeQuadResult |> Result.map .bottomLeft)
                , meterTriangleWithDefaults
                    "snoutSideTopMinor"
                    nymTemplate.coloring.snoutSideTopMinor
                    nymTemplate.structure.noseBridge
                    (eyeQuadResult |> Result.map .topLeft)
                    (eyeQuadResult |> Result.map .bottomLeft)
                ]

        snoutSideMiddle : Scene3d.Entity ()
        snoutSideMiddle =
            meterTriangleWithDefaults
                "snoutSideMiddle"
                nymTemplate.coloring.snoutSideMiddle
                nymTemplate.structure.noseTop
                nymTemplate.structure.cheekbone
                nymTemplate.structure.noseBottom

        snoutSideBottom : Scene3d.Entity ()
        snoutSideBottom =
            meterTriangleWithDefaults
                "snoutSideBottom"
                nymTemplate.coloring.snoutSideBottom
                nymTemplate.structure.cheekbone
                nymTemplate.structure.noseBottom
                nymTemplate.structure.jawPoint

        jawSide : Scene3d.Entity ()
        jawSide =
            meterTriangleWithDefaults
                "jawSide"
                nymTemplate.coloring.jawSide
                nymTemplate.structure.noseBottom
                nymTemplate.structure.chin
                nymTemplate.structure.jawPoint

        noseTip : Scene3d.Entity ()
        noseTip =
            meterQuadWithDefaults
                "noseTip"
                nymTemplate.coloring.noseTip
                nymTemplate.structure.noseBottom
                nymTemplate.structure.noseTop
                (nymTemplate.structure.noseTop |> Result.map mirrorPoint)
                (nymTemplate.structure.noseBottom |> Result.map mirrorPoint)

        mouth : Scene3d.Entity ()
        mouth =
            meterQuadWithDefaults
                "mouth"
                nymTemplate.coloring.mouth
                nymTemplate.structure.chin
                nymTemplate.structure.noseBottom
                (nymTemplate.structure.noseBottom |> Result.map mirrorPoint)
                (nymTemplate.structure.chin |> Result.map mirrorPoint)

        chinBottom : Scene3d.Entity ()
        chinBottom =
            meterQuadWithDefaults
                "chinBottom"
                nymTemplate.coloring.chinBottom
                nymTemplate.structure.jawPoint
                nymTemplate.structure.chin
                (nymTemplate.structure.chin |> Result.map mirrorPoint)
                (nymTemplate.structure.jawPoint |> Result.map mirrorPoint)

        neck : Scene3d.Entity ()
        neck =
            meterQuadWithDefaults
                "neck"
                nymTemplate.coloring.neck
                nymTemplate.structure.faceSideBottom
                nymTemplate.structure.jawPoint
                (nymTemplate.structure.jawPoint |> Result.map mirrorPoint)
                (nymTemplate.structure.faceSideBottom |> Result.map mirrorPoint)

        crown : Scene3d.Entity ()
        crown =
            meterQuadWithDefaults
                "crown"
                nymTemplate.coloring.crown
                nymTemplate.structure.crownFront
                nymTemplate.structure.crownBack
                (nymTemplate.structure.crownBack |> Result.map mirrorPoint)
                (nymTemplate.structure.crownFront |> Result.map mirrorPoint)

        crownSide : Scene3d.Entity ()
        crownSide =
            meterTriangleWithDefaults
                "crownSide"
                nymTemplate.coloring.crownSide
                nymTemplate.structure.crownBack
                nymTemplate.structure.crownFront
                nymTemplate.structure.faceSideTop

        aboveCheekbone : Scene3d.Entity ()
        aboveCheekbone =
            meterTriangleWithDefaults
                "aboveCheekbone"
                nymTemplate.coloring.aboveCheekbone
                (eyeQuadResult |> Result.map .bottomRight)
                (eyeQuadResult |> Result.map .topRight)
                nymTemplate.structure.cheekbone

        bridge : Scene3d.Entity ()
        bridge =
            meterQuadWithDefaults
                "bridge"
                nymTemplate.coloring.bridge
                nymTemplate.structure.noseBridge
                (eyeQuadResult |> Result.map .topLeft)
                (eyeQuadResult |> Result.map .topLeft |> Result.map mirrorPoint)
                (nymTemplate.structure.noseBridge |> Result.map mirrorPoint)

        forehead : Scene3d.Entity ()
        forehead =
            meterQuadWithDefaults
                "forehead"
                nymTemplate.coloring.forehead
                (eyeQuadResult |> Result.map .topLeft)
                nymTemplate.structure.crownFront
                (nymTemplate.structure.crownFront |> Result.map mirrorPoint)
                (eyeQuadResult |> Result.map .topLeft |> Result.map mirrorPoint)

        aboveEye : Scene3d.Entity ()
        aboveEye =
            meterTriangleWithDefaults
                "aboveEye"
                nymTemplate.coloring.aboveEye
                (eyeQuadResult |> Result.map .topLeft)
                nymTemplate.structure.crownFront
                (eyeQuadResult |> Result.map .topRight)

        belowEar : Scene3d.Entity ()
        belowEar =
            meterTriangleWithDefaults
                "belowEar"
                -- (Ok Color.blue)
                nymTemplate.coloring.belowEar
                (eyeQuadResult |> Result.map .topRight)
                nymTemplate.structure.crownFront
                nymTemplate.structure.faceSideTop

        faceSideTop : Scene3d.Entity ()
        faceSideTop =
            meterQuadWithDefaults
                "faceSideTop"
                -- (Ok Color.red)
                nymTemplate.coloring.faceSideTop
                (eyeQuadResult |> Result.map .topRight)
                nymTemplate.structure.faceSideTop
                nymTemplate.structure.faceSideMid
                nymTemplate.structure.cheekbone

        faceSideBottom : Scene3d.Entity ()
        faceSideBottom =
            meterQuadWithDefaults
                "faceSideBottom"
                nymTemplate.coloring.faceSideBottom
                nymTemplate.structure.faceSideBottom
                nymTemplate.structure.faceSideMid
                nymTemplate.structure.cheekbone
                nymTemplate.structure.jawPoint

        ear : Scene3d.Entity ()
        ear =
            Scene3d.group
                [ meterTriangleWithDefaults
                    "earBackOuter"
                    nymTemplate.coloring.earBackOuter
                    nymTemplate.structure.earAttachFrontBottom
                    nymTemplate.structure.earAttachBack
                    nymTemplate.structure.earTip
                , meterTriangleWithDefaults
                    "earBackInner"
                    nymTemplate.coloring.earBackInner
                    nymTemplate.structure.earAttachBack
                    nymTemplate.structure.earAttachFrontTop
                    nymTemplate.structure.earTip
                , meterTriangleWithDefaults
                    "earFrontOuter"
                    nymTemplate.coloring.earFrontOuter
                    nymTemplate.structure.earAttachFrontBottom
                    nymTemplate.structure.earAttachInside
                    nymTemplate.structure.earTip
                , meterTriangleWithDefaults
                    "earFrontInner"
                    nymTemplate.coloring.earFrontInner
                    nymTemplate.structure.earAttachInside
                    nymTemplate.structure.earAttachFrontTop
                    nymTemplate.structure.earTip
                ]

        testEntity =
            Scene3d.nothing

        -- testPoint nymTemplate.structure.jawPoint
    in
    allFeatures
        |> Scene3d.translateBy
            (nymBoundingBoxCenterPoint nymTemplate
                |> Result.withDefault (Vector3 0 0 0)
                |> Vector3.negate
                |> Vector3.toMetersVector
            )


nymBoundingBoxCenterPoint : NymTemplate -> Result GenError Vector3
nymBoundingBoxCenterPoint template =
    getBoundingBox template.structure
        |> Result.map
            (\( boundStart, boundEnd ) ->
                Vector3.midpoint boundStart boundEnd
            )


testPoint vectorResult =
    Scene3d.point
        { radius = Pixels.pixels 5 }
        (Material.color Color.black)
        (vectorResult |> Result.withDefault (Vector3 0 0 0) |> Vector3.toMetersPoint)


meterQuad : Color -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterQuad color v1 v2 v3 v4 =
    Scene3d.quad
        (Material.color color)
        (v1 |> Vector3.toMetersPoint)
        (v2 |> Vector3.toMetersPoint)
        (v3 |> Vector3.toMetersPoint)
        (v4 |> Vector3.toMetersPoint)


meterQuadWithDefaults :
    String
    -> Result GenError Color
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Scene3d.Entity ()
meterQuadWithDefaults name colorResult v1Result v2Result v3Result v4Result =
    Result.map4
        (\v1 v2 v3 v4 ->
            meterQuad
                (defaultAndLogColorError name colorResult)
                v1
                v2
                v3
                v4
        )
        v1Result
        v2Result
        v3Result
        v4Result
        |> defaultAndLogEntityError name


meterTriangle : Color -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterTriangle color v1 v2 v3 =
    Scene3d.triangle
        (Material.color color)
    <|
        Triangle3d.fromVertices
            ( v1 |> Vector3.toMetersPoint
            , v2 |> Vector3.toMetersPoint
            , v3 |> Vector3.toMetersPoint
            )


meterTriangleDebugLines : Color -> Vector3 -> Vector3 -> Vector3 -> Scene3d.Entity ()
meterTriangleDebugLines color v1 v2 v3 =
    [ ( v1, v2 )
    , ( v2, v3 )
    , ( v3, v1 )
    ]
        |> List.map (TupleHelpers.mapTuple2 Vector3.toMetersPoint)
        |> List.map LineSegment3d.fromEndpoints
        |> List.map
            (Scene3d.lineSegment
                (Material.color color)
            )
        |> Scene3d.group


meterTriangleWithDefaults :
    String
    -> Result GenError Color
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Result GenError Vector3
    -> Scene3d.Entity ()
meterTriangleWithDefaults name colorResult v1Result v2Result v3Result =
    Result.map3
        (\v1 v2 v3 ->
            meterTriangle
                (defaultAndLogColorError name colorResult)
                v1
                v2
                v3
        )
        v1Result
        v2Result
        v3Result
        |> defaultAndLogEntityError name


{-| Given a BinarySource, build a NymTemplate, but return a result
-}
binarySourceToNymTemplateResult : BinarySource -> Result ( NymTemplate, GenError ) Nym
binarySourceToNymTemplateResult =
    binarySourceToNymTemplate
        >> TupleHelpers.tuple3Last
        >> (\template ->
                finalizeTemplate template
                    |> Result.mapError (Tuple.pair template)
           )


{-| Given a BinarySource, build a NymTemplate
-}
binarySourceToNymTemplate : BinarySource -> ( String, ( Int, List Int ), NymTemplate )
binarySourceToNymTemplate source =
    let
        ( rSource1, coreStructureTemplate, structureDemarcatePositions ) =
            Generate.consumeCoreStructureToTemplate source

        ( rSource2, coloringTemplate, coloringDemarcatePositions ) =
            Generate.consumeColoringToTemplate rSource1

        bitsLeft =
            BinarySource.remainingBits rSource2

        demarcatePositions =
            structureDemarcatePositions
                ++ (coloringDemarcatePositions
                        |> List.map
                            ((+)
                                (List.Extra.last structureDemarcatePositions |> Maybe.withDefault 0)
                            )
                   )
    in
    ( source
        |> BinarySource.getBitsString
        |> String.dropRight bitsLeft
    , ( bitsLeft, demarcatePositions )
    , NymTemplate
        coreStructureTemplate
        coloringTemplate
    )


defaultAndLogEntityError : String -> Result GenError (Scene3d.Entity ()) -> Scene3d.Entity ()
defaultAndLogEntityError name =
    Result.Extra.extract
        (\err ->
            let
                _ =
                    Debug.log ("Entity " ++ name ++ " failed") err
            in
            Scene3d.nothing
        )


defaultAndLogColorError : String -> Result GenError Color -> Color
defaultAndLogColorError name =
    Result.Extra.unpack
        (\err ->
            let
                _ =
                    Debug.log ("Color " ++ name ++ " failed") err
            in
            Color.black
        )
        identity


finalizeTemplate : NymTemplate -> Result GenError Nym
finalizeTemplate template =
    Result.map2 Tuple.pair
        (Result.map3 TupleHelpers.tuple3
            (Result.map3 TupleHelpers.tuple3
                (Result.map3 TupleHelpers.tuple3
                    template.structure.eyeQuadInfo
                    template.structure.noseTop
                    template.structure.noseBridge
                )
                (Result.map3 TupleHelpers.tuple3
                    template.structure.noseBottom
                    template.structure.cheekbone
                    template.structure.crownFront
                )
                (Result.map3 TupleHelpers.tuple3
                    template.structure.crownBack
                    template.structure.backZ
                    template.structure.faceSideTop
                )
            )
            (Result.map3 TupleHelpers.tuple3
                (Result.map3 TupleHelpers.tuple3
                    template.structure.faceSideMid
                    template.structure.faceSideBottom
                    template.structure.jawPoint
                )
                (Result.map3 TupleHelpers.tuple3
                    template.structure.chin
                    template.structure.earAttachFrontTop
                    template.structure.earAttachFrontBottom
                )
                (Result.map3 TupleHelpers.tuple3
                    template.structure.earBaseNormal
                    template.structure.earAttachBack
                    template.structure.earAttachInside
                )
            )
            (Result.map3 TupleHelpers.tuple3
                (Result.map3 TupleHelpers.tuple3
                    template.structure.earTip
                    template.coloring.snoutTop
                    template.coloring.snoutSideTopMajor
                )
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.snoutSideTopMinor
                    template.coloring.snoutSideMiddle
                    template.coloring.noseTip
                )
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.aboveCheekbone
                    template.coloring.bridge
                    template.coloring.forehead
                )
            )
        )
        (Result.map2 Tuple.pair
            (Result.map3 TupleHelpers.tuple3
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.aboveEye
                    template.coloring.eyeQuad
                    template.coloring.belowEar
                )
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.faceSideTop
                    template.coloring.faceSideBottom
                    template.coloring.snoutSideBottom
                )
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.jawSide
                    template.coloring.mouth
                    template.coloring.chinBottom
                )
            )
            (Result.map3 TupleHelpers.tuple3
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.neck
                    template.coloring.crown
                    template.coloring.crownSide
                )
                (Result.map3 TupleHelpers.tuple3
                    template.coloring.earBackOuter
                    template.coloring.earBackInner
                    template.coloring.earFrontOuter
                )
                template.coloring.earFrontInner
            )
        )
        |> Result.map
            (\( ( ( ( structureEyeQuadInfo, structureNoseTop, structureNoseBridge ), ( structureNoseBottom, structureCheekbone, structureCrownFront ), ( structureCrownBack, structureBackZ, structureFaceSideTop ) ), ( ( structureFaceSideMid, structureFaceSideBottom, structureJawPoint ), ( structureChin, structureEarAttachFrontTop, structureEarAttachFrontBottom ), ( structureEarBaseNormal, structureEarAttachBack, structureEarAttachInside ) ), ( ( structureEarTip, coloringSnoutTop, coloringSnoutSideTopMajor ), ( coloringSnoutSideTopMinor, coloringSnoutSideMiddle, coloringNoseTip ), ( coloringAboveCheekbone, coloringBridge, coloringForehead ) ) ), ( ( ( coloringAboveEye, coloringEyeQuad, coloringBelowEar ), ( coloringFaceSideTop, coloringFaceSideBottom, coloringSnoutSideBottom ), ( coloringJawSide, coloringMouth, coloringChinBottom ) ), ( ( coloringNeck, coloringCrown, coloringCrownSide ), ( coloringEarBackOuter, coloringEarBackInner, coloringEarFrontOuter ), coloringEarFrontInner ) ) ) ->
                Nym
                    { eyeQuadInfo = structureEyeQuadInfo
                    , noseTop = structureNoseTop
                    , noseBridge = structureNoseBridge
                    , noseBottom = structureNoseBottom
                    , cheekbone = structureCheekbone
                    , crownFront = structureCrownFront
                    , crownBack = structureCrownBack
                    , backZ = structureBackZ
                    , faceSideTop = structureFaceSideTop
                    , faceSideMid = structureFaceSideMid
                    , faceSideBottom = structureFaceSideBottom
                    , jawPoint = structureJawPoint
                    , chin = structureChin
                    , earAttachFrontTop = structureEarAttachFrontTop
                    , earAttachFrontBottom = structureEarAttachFrontBottom
                    , earBaseNormal = structureEarBaseNormal
                    , earAttachBack = structureEarAttachBack
                    , earAttachInside = structureEarAttachInside
                    , earTip = structureEarTip
                    }
                    { snoutTop = coloringSnoutTop
                    , snoutSideTopMajor = coloringSnoutSideTopMajor
                    , snoutSideTopMinor = coloringSnoutSideTopMinor
                    , snoutSideMiddle = coloringSnoutSideMiddle
                    , noseTip = coloringNoseTip
                    , aboveCheekbone = coloringAboveCheekbone
                    , bridge = coloringBridge
                    , forehead = coloringForehead
                    , aboveEye = coloringAboveEye
                    , eyeQuad = coloringEyeQuad
                    , belowEar = coloringBelowEar
                    , faceSideTop = coloringFaceSideTop
                    , faceSideBottom = coloringFaceSideBottom
                    , snoutSideBottom = coloringSnoutSideBottom
                    , jawSide = coloringJawSide
                    , mouth = coloringMouth
                    , chinBottom = coloringChinBottom
                    , neck = coloringNeck
                    , crown = coloringCrown
                    , crownSide = coloringCrownSide
                    , earBackOuter = coloringEarBackOuter
                    , earBackInner = coloringEarBackInner
                    , earFrontOuter = coloringEarFrontOuter
                    , earFrontInner = coloringEarFrontInner
                    }
            )


nymToOkTemplate : Nym -> NymTemplate
nymToOkTemplate nym =
    NymTemplate
        { eyeQuadInfo = Ok nym.structure.eyeQuadInfo
        , noseTop = Ok nym.structure.noseTop
        , noseBridge = Ok nym.structure.noseBridge
        , noseBottom = Ok nym.structure.noseBottom
        , cheekbone = Ok nym.structure.cheekbone
        , crownFront = Ok nym.structure.crownFront
        , crownBack = Ok nym.structure.crownBack
        , backZ = Ok nym.structure.backZ
        , faceSideTop = Ok nym.structure.faceSideTop
        , faceSideMid = Ok nym.structure.faceSideMid
        , faceSideBottom = Ok nym.structure.faceSideBottom
        , jawPoint = Ok nym.structure.jawPoint
        , chin = Ok nym.structure.chin
        , earAttachFrontTop = Ok nym.structure.earAttachFrontTop
        , earAttachFrontBottom = Ok nym.structure.earAttachFrontBottom
        , earBaseNormal = Ok nym.structure.earBaseNormal
        , earAttachBack = Ok nym.structure.earAttachBack
        , earAttachInside = Ok nym.structure.earAttachInside
        , earTip = Ok nym.structure.earTip
        }
        { snoutTop = Ok nym.coloring.snoutTop
        , snoutSideTopMajor = Ok nym.coloring.snoutSideTopMajor
        , snoutSideTopMinor = Ok nym.coloring.snoutSideTopMinor
        , snoutSideMiddle = Ok nym.coloring.snoutSideMiddle
        , noseTip = Ok nym.coloring.noseTip
        , aboveCheekbone = Ok nym.coloring.aboveCheekbone
        , bridge = Ok nym.coloring.bridge
        , forehead = Ok nym.coloring.forehead
        , aboveEye = Ok nym.coloring.aboveEye
        , eyeQuad = Ok nym.coloring.eyeQuad
        , belowEar = Ok nym.coloring.belowEar
        , faceSideTop = Ok nym.coloring.faceSideTop
        , faceSideBottom = Ok nym.coloring.faceSideBottom
        , snoutSideBottom = Ok nym.coloring.snoutSideBottom
        , jawSide = Ok nym.coloring.jawSide
        , mouth = Ok nym.coloring.mouth
        , chinBottom = Ok nym.coloring.chinBottom
        , neck = Ok nym.coloring.neck
        , crown = Ok nym.coloring.crown
        , crownSide = Ok nym.coloring.crownSide
        , earBackOuter = Ok nym.coloring.earBackOuter
        , earBackInner = Ok nym.coloring.earBackInner
        , earFrontOuter = Ok nym.coloring.earFrontOuter
        , earFrontInner = Ok nym.coloring.earFrontInner
        }


{-| temp
-}
type alias EyeQuadAndPupil2d =
    { pupil : Pupil2d
    , eyeQuad : EyeQuad2d
    }


{-| temp
-}
type alias Pupil2d =
    List ( Vector2, Vector2, Vector2 )


{-| temp
-}
type alias EyeQuad2d =
    { bottomRight : Vector2
    , bottomLeft : Vector2
    , topLeft : Vector2
    , topRight : Vector2
    }
