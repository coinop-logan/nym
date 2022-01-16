module Types exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Length
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Result.Extra
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


type GenError
    = NotEnoughSource
    | NotYetSet
    | UnexpectedNothing String


type alias Point3dM =
    Point3d Length.Meters ()


type alias Structure =
    { eyeQuadInfo : EyeQuadInfo
    , noseTop : Vector3
    , noseBridge : Vector3
    , noseBottom : Vector3
    , cheekbone : Vector3
    , crownFront : Vector3
    , crownBack : Vector3
    , backZ : Float
    , faceSideTop : Vector3
    , faceSideMid : Vector3
    , faceSideBottom : Vector3
    , jawPoint : Vector3
    , chin : Vector3
    , earAttachFrontTop : Vector3
    , earAttachFrontBottom : Vector3
    , earBaseNormal : Direction3d ()
    , earAttachBack : Vector3
    , earAttachInside : Vector3
    , earTip : Vector3
    }


type alias StructureTemplate =
    { eyeQuadInfo : Result GenError EyeQuadInfo
    , noseTop : Result GenError Vector3
    , noseBridge : Result GenError Vector3
    , noseBottom : Result GenError Vector3
    , cheekbone : Result GenError Vector3
    , crownFront : Result GenError Vector3
    , crownBack : Result GenError Vector3
    , backZ : Result GenError Float
    , faceSideTop : Result GenError Vector3
    , faceSideMid : Result GenError Vector3
    , faceSideBottom : Result GenError Vector3
    , jawPoint : Result GenError Vector3
    , chin : Result GenError Vector3
    , earAttachFrontTop : Result GenError Vector3
    , earAttachFrontBottom : Result GenError Vector3
    , earBaseNormal : Result GenError (Direction3d ())
    , earAttachBack : Result GenError Vector3
    , earAttachInside : Result GenError Vector3
    , earTip : Result GenError Vector3
    }


type alias EyeQuadInfo =
    { sketchPlane : SketchPlane3d Length.Meters () {}
    , eyeQuad : Vector3.Quad
    , pupil : Pupil
    }


type alias Pupil =
    List ( Vector3, Vector3, Vector3 )


type alias EyeQuadAndPupil2d =
    { pupil : Pupil2d
    , eyeQuad : Vector2.Quad
    }


type alias Pupil2d =
    List ( Vector2, Vector2, Vector2 )


type alias Coloring =
    { snoutTop : Color
    , snoutSideTopMajor : Color
    , snoutSideTopMinor : Color
    , snoutSideMiddle : Color
    , noseTip : Color
    , aboveCheekbone : Color
    , bridge : Color
    , forehead : Color
    , aboveEye : Color
    , eyeQuad : Color
    , belowEar : Color
    , faceSideTop : Color
    , faceSideBottom : Color
    , snoutSideBottom : Color
    , jawSide : Color
    , mouth : Color
    , chinBottom : Color
    , neck : Color
    , crown : Color
    , crownSide : Color
    , earBackOuter : Color
    , earBackInner : Color
    , earFrontOuter : Color
    , earFrontInner : Color
    }


type alias ColoringTemplate =
    { snoutTop : Result GenError Color
    , snoutSideTopMajor : Result GenError Color
    , snoutSideTopMinor : Result GenError Color
    , snoutSideMiddle : Result GenError Color
    , noseTip : Result GenError Color
    , aboveCheekbone : Result GenError Color
    , bridge : Result GenError Color
    , forehead : Result GenError Color
    , aboveEye : Result GenError Color
    , eyeQuad : Result GenError Color
    , belowEar : Result GenError Color
    , faceSideTop : Result GenError Color
    , faceSideBottom : Result GenError Color
    , snoutSideBottom : Result GenError Color
    , jawSide : Result GenError Color
    , mouth : Result GenError Color
    , chinBottom : Result GenError Color
    , neck : Result GenError Color
    , crown : Result GenError Color
    , crownSide : Result GenError Color
    , earBackOuter : Result GenError Color
    , earBackInner : Result GenError Color
    , earFrontOuter : Result GenError Color
    , earFrontInner : Result GenError Color
    }


type alias EyeTemplate =
    Result GenError Vector3


type alias Eye =
    Point3dM


testEye : Eye
testEye =
    Point3d.meters 0.3 0.05 0.4


type alias NymTemplate =
    { structure : StructureTemplate
    , coloring : ColoringTemplate
    }


type alias Nym =
    { structure : Structure
    , coloring : Coloring
    }


squashMaybe : String -> a -> Maybe a -> a
squashMaybe warning default maybeVal =
    case maybeVal of
        Just a ->
            a

        Nothing ->
            let
                _ =
                    Debug.log "maybe squashed:" warning
            in
            default


type alias Transformer templateType =
    templateType -> templateType


type alias TransformerGenResult templateType =
    Result GenError (Transformer templateType)


type alias IndexedTransformGenerator templateType =
    BinarySource -> Int -> ( BinarySource, TransformerGenResult templateType )



