module Transforms exposing (..)

import BinarySource exposing (BinarySource)
import Color exposing (Color)
import List
import List.Extra
import Maybe.Extra
import Point3d exposing (Point3d)
import Result.Extra
import TupleHelpers
import Types exposing (..)
import Utils exposing (..)
import Vector3 exposing (Vector3)
import Vector3d


coreStructureTransforms : List (BinarySource -> BaseStructureTemplate -> ( BinarySource, BaseStructureTemplate ))
coreStructureTransforms =
    [ \source template ->
        -- crownBack X
        source
            -- 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
            |> BinarySource.consumeFloatRange 2
                ( 0.2, 0.7 )
            |> tryApplyToTemplate
                (\xResult ->
                    { template
                        | crownBack =
                            Result.map
                                (\x ->
                                    Vector3
                                        x
                                        1
                                        0
                                )
                                xResult
                    }
                )
    , \source template ->
        -- crownFront
        source
            |> BinarySource.consumeVectorFromBounds 2
                ( Vector3 0.2 1 0.2
                , Vector3 0.7 0.7 0.4
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | crownFront = pointResult
                    }
                )
    , \source template ->
        -- brow
        source
            |> BinarySource.consumeVectorFromBounds 2
                ( Vector3 0.2 0.7 0.2
                , Vector3 0.6 0.1 0.4
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | brow = pointResult
                    }
                )
    , \source template ->
        -- outerTop X and Y
        source
            |> BinarySource.consume2
                ( BinarySource.consumeFloatRange 2
                    ( 0.7, 1 )
                , BinarySource.consumeFloatRange 2
                    ( 0, 0.7 )
                )
            |> BinarySource.map
                (\( x, y ) ->
                    Vector3 x y 0
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | outerTop = pointResult
                    }
                )
    , \source template ->
        -- jawBottom x and y
        source
            |> BinarySource.consume2
                ( BinarySource.consumeFloatRange 2
                    ( 0.4, 1 )
                , BinarySource.consumeFloatRange 2
                    ( -0.7, -1 )
                )
            |> BinarySource.map
                (\( x, y ) ->
                    Vector3 x y 0
                )
            |> tryApplyToTemplate
                (\pointResult ->
                    { template
                        | jawBottom = pointResult
                    }
                )

    -- , \source template ->
    --     -- nose y and z
    --     source
    --         |> BinarySource.consume2
    --             ( BinarySource.consumeFloatRange 2
    --                 ()
    --             , BinarySource.consumeFloatRange 2
    --             )
    --   0.2 -1 0.9
    -- mouthCorner   0.23 -0.9 1
    -- noseTip   0.18 -0.8 1
    -- brow   0.3 0.4 0.3
    ]


coloringTransforms : List (BinarySource -> ColoringTemplate -> ( BinarySource, ColoringTemplate ))
coloringTransforms =
    []
    -- [ \source template ->
    --     source
    --         |> BinarySource.consumeColorFromPallette
    --         |> tryApplyToTemplate
    --             (\colorResult ->
    --                 { template
    --                     | crown =
    --                         colorResult
    --                 }
    --             )
    -- ]


tryApplyToTemplate :
    (Result GenError val -> template)
    -> Maybe ( BinarySource, val )
    -> ( BinarySource, template )
tryApplyToTemplate func maybeSourceAndVal =
    let
        result =
            maybeSourceAndVal
                |> Maybe.map Tuple.second
                |> Result.fromMaybe NotEnoughSource

        remainingSource =
            maybeSourceAndVal
                |> Maybe.map Tuple.first
                |> Maybe.withDefault BinarySource.empty
    in
    ( remainingSource
    , func result
    )
