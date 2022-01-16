module BinarySource exposing
    ( BinaryChunk
    , BinarySource
    , allColors
    , andThenConsume
    , consume2
    , consume3
    , consumeBool
    , consumeChunk
    , consumeColorFromPallette
    , consumeDouble
    , consumeFloat0to1
    , consumeFloatRange
    , consumeInt
    , consumeTriple
    , consumeUnsignedFloat
    , consumeVector3
    , consumeVector3ByComponent
    , consumeVector3DimNeg1to1
    , consumeVector3FromBounds
    , cycle
    , cycleWithSalt
    , debugLogAboutToConsume
    , empty
    , emptyConsume
    , fromBigInt
    , fromBitsString
    , fromHexString
    , fromIntString
    , fromUint256Hex
    , getBitsString
    , map
    , remainingBits
    , seedTo256Bits
    )

import BigInt exposing (BigInt)
import Binary exposing (Bits)
import Color exposing (Color)
import Crypto.Hash as Hash
import Hex
import List.Extra
import Maybe.Extra
import String
import TupleHelpers
import UInt64
import UInt64.Digits as UInt64
import Vector3 exposing (Vector3)
import Vector3d exposing (Vector3d)


type BinarySource
    = BinarySource Bits


type BinaryChunk
    = BinaryChunk Bits


empty : BinarySource
empty =
    BinarySource Binary.empty


seedTo256Bits : String -> BinarySource
seedTo256Bits =
    Hash.sha256
        >> fromHexString
        >> Maybe.withDefault empty


cycle : BinarySource -> BinarySource
cycle =
    getHexString
        >> Hash.sha256
        >> fromHexString
        >> Maybe.withDefault empty


cycleWithSalt : String -> BinarySource -> BinarySource
cycleWithSalt salt =
    getHexString
        >> (++) salt
        >> Hash.sha256
        >> fromHexString
        >> Maybe.withDefault empty


debugLogAboutToConsume : BinarySource -> BinarySource
debugLogAboutToConsume s =
    let
        _ =
            Debug.log "about to consume" (getBitsString s)
    in
    s


getBitsString : BinarySource -> String
getBitsString (BinarySource s) =
    Binary.toIntegers s
        |> List.map
            (\c ->
                if c == 1 then
                    '1'

                else
                    '0'
            )
        |> String.fromList


getHexString : BinarySource -> String
getHexString (BinarySource s) =
    Binary.toHex s


fromBitsString : String -> BinarySource
fromBitsString =
    bitsStringToBooleans
        >> Binary.fromBooleans
        >> BinarySource


bitsStringToBooleans : String -> List Bool
bitsStringToBooleans =
    String.toList
        >> List.map
            (\c ->
                if c == '0' then
                    False

                else
                    True
            )


booleansToBitsString : List Bool -> String
booleansToBitsString =
    List.map
        (\b ->
            if b then
                '1'

            else
                '0'
        )
        >> String.fromList


fromIntegers : List Int -> BinarySource
fromIntegers =
    BinarySource << Binary.fromIntegers


fromUint256Hex : String -> Maybe BinarySource
fromUint256Hex =
    String.padLeft 32 '0'
        >> fromHexString


fromHexString : String -> Maybe BinarySource
fromHexString =
    String.toList
        >> List.map hexCharToPaddedBitsString
        >> Maybe.Extra.combine
        >> Maybe.map String.concat
        >> Maybe.map fromBitsString


fromIntString : String -> Maybe BinarySource
fromIntString =
    BigInt.fromIntString
        >> Maybe.map BigInt.toHexString
        >> Maybe.andThen fromHexString


fromBigInt : BigInt -> BinarySource
fromBigInt =
    BigInt.toHexString
        >> String.padLeft 64 '0'
        >> Binary.fromHex
        >> BinarySource


hexCharToPaddedBitsString : Char -> Maybe String
hexCharToPaddedBitsString =
    String.fromChar
        >> String.toLower
        >> Hex.fromString
        >> Result.toMaybe
        >> Maybe.map
            (Binary.fromDecimal
                >> Binary.toBooleans
                >> List.map
                    (\b ->
                        if b then
                            '1'

                        else
                            '0'
                    )
                >> String.fromList
                >> String.padLeft 4 '0'
            )


remainingBits : BinarySource -> Int
remainingBits source =
    String.length <| getBitsString source


consumeChunk : Int -> BinarySource -> Maybe ( BinarySource, BinaryChunk, Int )
consumeChunk numBits source =
    let
        bitsString =
            getBitsString source
    in
    if String.length bitsString >= numBits then
        Just
            ( fromBitsString <| String.dropLeft numBits bitsString
            , BinaryChunk <| Binary.fromBooleans <| bitsStringToBooleans <| String.left numBits bitsString
            , numBits
            )

    else
        Nothing


consumeBool : BinarySource -> Maybe ( BinarySource, Bool, Int )
consumeBool source =
    consumeInt 1 source
        |> map (\i -> i == 1)


consumeInt : Int -> BinarySource -> Maybe ( BinarySource, Int, Int )
consumeInt bits source =
    source
        |> consumeChunk bits
        |> Maybe.map (TupleHelpers.mapTuple3Middle chunkToInt32)


consumeFloat0to1 : Int -> BinarySource -> Maybe ( BinarySource, Float, Int )
consumeFloat0to1 bits source =
    source
        |> consumeInt bits
        |> Maybe.map
            (TupleHelpers.mapTuple3Middle
                (\divisorInt ->
                    toFloat divisorInt
                        / (toFloat <| 2 ^ bits - 1)
                )
            )


consumeUnsignedFloat : Int -> Float -> BinarySource -> Maybe ( BinarySource, Float, Int )
consumeUnsignedFloat bits max =
    consumeFloat0to1 bits
        >> map ((*) max)


consumeFloatRange : Int -> ( Float, Float ) -> BinarySource -> Maybe ( BinarySource, Float, Int )
consumeFloatRange bits ( min, max ) =
    consumeFloat0to1 bits
        >> map
            (\uFloat ->
                uFloat * (max - min) + min
            )


consumeVector3DimNeg1to1 : Int -> BinarySource -> Maybe ( BinarySource, Vector3, Int )
consumeVector3DimNeg1to1 bitsPerComponent source =
    consumeFloat0to1 bitsPerComponent source
        |> Maybe.andThen
            (\( source1, x, bitsUsed1 ) ->
                consumeFloat0to1 bitsPerComponent source1
                    |> Maybe.andThen
                        (\( source2, y, bitsUsed2 ) ->
                            consumeFloat0to1 bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( source3, z, bitsUsed3 ) ->
                                        Just
                                            ( source3
                                            , Vector3 x y z
                                                |> Vector3.scaleBy 2
                                                |> Vector3.minus (Vector3 1 1 1)
                                            , bitsUsed1 + bitsUsed2 + bitsUsed3
                                            )
                                    )
                        )
            )


consumeVector3ByComponent : ( ( Int, Float, Float ), ( Int, Float, Float ), ( Int, Float, Float ) ) -> BinarySource -> Maybe ( BinarySource, Vector3, Int )
consumeVector3ByComponent componentConsumeInfos source =
    let
        consumeFuncs =
            componentConsumeInfos
                |> TupleHelpers.mapTuple3
                    (\( bits, min, max ) ->
                        consumeFloatRange bits ( min, max )
                    )
    in
    consume3 consumeFuncs source
        |> map (\( x, y, z ) -> Vector3 x y z)


consumeVector3FromBounds : Int -> Vector3.RectBounds -> BinarySource -> Maybe ( BinarySource, Vector3, Int )
consumeVector3FromBounds bitsPerComponent ( boundsStart, boundsEnd ) source =
    consumeFloat0to1 bitsPerComponent source
        |> Maybe.andThen
            (\( source1, x, bitsUsed1 ) ->
                consumeFloat0to1 bitsPerComponent source1
                    |> Maybe.andThen
                        (\( source2, y, bitsUsed2 ) ->
                            consumeFloat0to1 bitsPerComponent source2
                                |> Maybe.andThen
                                    (\( source3, z, bitsUsed3 ) ->
                                        let
                                            boundsSpace =
                                                boundsEnd |> Vector3.minus boundsStart
                                        in
                                        Just
                                            ( source3
                                            , Vector3 x y z
                                                |> Vector3.scaleByVector boundsSpace
                                                |> Vector3.plus boundsStart
                                            , bitsUsed1 + bitsUsed2 + bitsUsed3
                                            )
                                    )
                        )
            )


consumeVector3 : Int -> Vector3 -> BinarySource -> Maybe ( BinarySource, Vector3, Int )
consumeVector3 bitsPerComponent maxVector =
    consumeVector3FromBounds bitsPerComponent
        ( Vector3.zero
        , maxVector
        )


chunkToInt32 : BinaryChunk -> Int
chunkToInt32 chunk =
    chunk
        |> encodeBinaryString
        |> UInt64.fromString
        |> Maybe.withDefault UInt64.maxSafe
        -- so failure is as ugly and noticeable as possible :D Better than a silent zero...
        |> UInt64.toInt32s
        |> Tuple.second


encodeBinaryString : BinaryChunk -> String
encodeBinaryString (BinaryChunk chunk) =
    "0b" ++ (booleansToBitsString <| Binary.toBooleans chunk)


consumeColorFromPallette : BinarySource -> Maybe ( BinarySource, Color, Int )
consumeColorFromPallette source =
    consumeInt 5 source
        -- happily the list contains exactly 32 items, so 5 bits is perfect
        |> Maybe.map
            (TupleHelpers.mapTuple3Middle
                (\colorNum ->
                    List.Extra.getAt colorNum allColors
                )
            )
        |> (\weirdMaybe ->
                case weirdMaybe of
                    Just ( a, Just b, c ) ->
                        Just ( a, b, c )

                    _ ->
                        Nothing
           )


consume3 :
    ( BinarySource -> Maybe ( BinarySource, a, Int )
    , BinarySource -> Maybe ( BinarySource, b, Int )
    , BinarySource -> Maybe ( BinarySource, c, Int )
    )
    -> BinarySource
    -> Maybe ( BinarySource, ( a, b, c ), Int )
consume3 ( f1, f2, f3 ) source =
    source
        |> f1
        |> Maybe.andThen
            (\( s1, v1, b1 ) ->
                s1
                    |> f2
                    |> Maybe.andThen
                        (\( s2, v2, b2 ) ->
                            s2
                                |> f3
                                |> Maybe.map
                                    (\( s3, v3, b3 ) ->
                                        ( s3
                                        , ( v1, v2, v3 )
                                        , b1 + b2 + b3
                                        )
                                    )
                        )
            )


consumeTriple : (BinarySource -> Maybe ( BinarySource, a, Int )) -> BinarySource -> Maybe ( BinarySource, ( a, a, a ), Int )
consumeTriple f source =
    consume3 ( f, f, f ) source


consume2 :
    ( BinarySource -> Maybe ( BinarySource, a, Int )
    , BinarySource -> Maybe ( BinarySource, b, Int )
    )
    -> BinarySource
    -> Maybe ( BinarySource, ( a, b ), Int )
consume2 ( f1, f2 ) source =
    source
        |> f1
        |> Maybe.andThen
            (\( s1, v1, b1 ) ->
                s1
                    |> f2
                    |> Maybe.map
                        (\( s2, v2, b2 ) ->
                            ( s2
                            , ( v1, v2 )
                            , b1 + b2
                            )
                        )
            )


consumeDouble : (BinarySource -> Maybe ( BinarySource, a, Int )) -> BinarySource -> Maybe ( BinarySource, ( a, a ), Int )
consumeDouble f source =
    consume2 ( f, f ) source



-- consumeSeveralValues : Int -> (BinarySource -> Maybe ( BinarySource, valType, Int )) -> BinarySource -> Maybe ( BinarySource, List valType, Int )
-- consumeSeveralValues count f source =
--     let
--         func : BinarySource -> () -> ( BinarySource, Maybe valType, Int )
--         func s _ =
--             f s
--                 |> Maybe.Extra.unwrap
--                     ( s, Nothing, 0 )
--                     (TupleHelpers.mapTuple3Middle Just)
--         ( remainingSource, maybeListOfVals, bitsUsed ) =
--             List.Extra.mapAccuml
--                 func
--                 source
--                 (List.repeat count ())
--                 |> TupleHelpers.mapTuple3Middle Maybe.Extra.combine
--     in
--     maybeListOfVals
--         |> Maybe.map
--             (\vals ->
--                 ( remainingSource, vals )
--             )


map : (a -> b) -> Maybe ( BinarySource, a, Int ) -> Maybe ( BinarySource, b, Int )
map f maybeSourceAndVal =
    maybeSourceAndVal
        |> Maybe.map (TupleHelpers.mapTuple3Middle f)


andThenConsume : (BinarySource -> Maybe ( BinarySource, b, Int )) -> (a -> b -> c) -> Maybe ( BinarySource, a, Int ) -> Maybe ( BinarySource, c, Int )
andThenConsume consumeFunc mapFunc maybeSourceAndVal =
    maybeSourceAndVal
        |> Maybe.andThen
            (\( source1, aVal, bitsUsed1 ) ->
                consumeFunc source1
                    |> Maybe.map
                        (\( source2, bVal, bitsUsed2 ) ->
                            ( source2
                            , mapFunc aVal bVal
                            , bitsUsed1 + bitsUsed2
                            )
                        )
            )


emptyConsume : a -> BinarySource -> Maybe ( BinarySource, a, Int )
emptyConsume val source =
    Just ( source, val, 0 )



-- consumeSeveralValues : BinarySource -> Int -> (BinarySource -> Maybe ( BinarySource, valType )) -> Maybe ( BinarySource, List valType )
-- consumeSeveralValues source count f =
--     List.range 0 count
--         |> List.Extra.mapAccuml
--             func
--             source
--     let
--         trfunc : () -> Maybe ( BinarySource, List valType ) -> Maybe ( BinarySource, List valType )
--         trfunc _ ( s, vals ) =
--             if List.length vals >= count then
--                 Nothing
--             else
--                 f s
--                     |> Maybe.map
--                         (\(newSource, newVal) ->
--                         )
--         func : ( BinarySource, Int ) -> Maybe ( valType, ( BinarySource, Int ) )
--         func ( s, i ) =
--             if i >= count then
--                 Nothing
--             else
--                 f s
--                     |> Maybe.map
--                         (\( newSource, newVal ) ->
--                             ( newVal, ( newSource, i + 1 ) )
--                         )
--         vals =
--             List.Extra.unfoldr
--                 func
--                 ( source, 0 )
--     in
--     if List.length vals < count then
--         Nothing
--     else
--         Just vals


allColors =
    [ Color.lightRed
    , Color.red
    , Color.darkRed
    , Color.lightOrange
    , Color.orange
    , Color.darkOrange
    , Color.lightYellow
    , Color.yellow
    , Color.darkYellow
    , Color.lightGreen
    , Color.green
    , Color.darkGreen
    , Color.lightBlue
    , Color.blue
    , Color.darkBlue
    , Color.lightPurple
    , Color.purple
    , Color.darkPurple
    , Color.lightBrown
    , Color.brown
    , Color.darkBrown
    , Color.black
    , Color.white
    , Color.lightGrey
    , Color.grey
    , Color.darkGrey
    , Color.lightGray
    , Color.gray
    , Color.darkGray
    , Color.lightCharcoal
    , Color.charcoal
    , Color.darkCharcoal
    ]
