module Generate exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Point3d exposing (Point3d)
import Result.Extra
import Transforms
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


blankColoringTemplate : ColoringTemplate
blankColoringTemplate =
    ColoringTemplate
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)


blankStructureTemplate : StructureTemplate
blankStructureTemplate =
    StructureTemplate
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)
        (Err NotYetSet)


blankEyeTemplate : EyeTemplate
blankEyeTemplate =
    Err NotYetSet


consumeColoringToTemplate : BinarySource -> ( BinarySource, ColoringTemplate, List Int )
consumeColoringToTemplate fullSource =
    let
        trfunc :
            (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate, Int ))
            -> ( BinarySource, ColoringTemplate, List Int )
            -> ( BinarySource, ColoringTemplate, List Int )
        trfunc transform ( source, template, demarcatePositions ) =
            let
                ( newSource, newTemplate, bitsUsed ) =
                    transform source template

                newDemarcatePositions =
                    List.append demarcatePositions
                        [ bitsUsed + (List.Extra.last demarcatePositions |> Maybe.withDefault 0) ]
            in
            ( newSource, newTemplate, newDemarcatePositions )

        remainingSourceAndTemplate =
            List.foldl
                trfunc
                ( fullSource, blankColoringTemplate, [] )
                Transforms.coloringTransforms
    in
    remainingSourceAndTemplate


consumeCoreStructureToTemplate : BinarySource -> ( BinarySource, StructureTemplate, List Int )
consumeCoreStructureToTemplate fullSource =
    let
        trfunc :
            (BinarySource -> StructureTemplate -> ( BinarySource, StructureTemplate, Int ))
            -> ( BinarySource, StructureTemplate, List Int )
            -> ( BinarySource, StructureTemplate, List Int )
        trfunc transform ( source, template, demarcatePositions ) =
            let
                ( newSource, newTemplate, bitsUsed ) =
                    transform source template

                newDemarcatePositions =
                    List.append demarcatePositions
                        [ bitsUsed + (List.Extra.last demarcatePositions |> Maybe.withDefault 0) ]
            in
            ( newSource, newTemplate, newDemarcatePositions )
    in
    List.foldl
        trfunc
        ( fullSource, blankStructureTemplate, [] )
        Transforms.coreStructureTransforms


consumeEyeToTemplate : BinarySource -> ( BinarySource, EyeTemplate )
consumeEyeToTemplate source =
    ( source, blankEyeTemplate )
