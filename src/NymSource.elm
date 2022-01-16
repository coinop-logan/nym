module NymSource exposing (Source, fromUintId)

{-| Blarg.

@docs Source, fromUintId

-}

import BigInt exposing (BigInt)
import BinarySource exposing (BinarySource)


{-| Represents the "genetic code" of the Nym. Use `fromUintId` to generate.
-}
type alias Source =
    BinarySource


{-| Generate a Source from a BigInt (assumed to be the Nym NFT identifier in uint form)
-}
fromUintId : BigInt -> BinarySource
fromUintId =
    BinarySource.fromBigInt
