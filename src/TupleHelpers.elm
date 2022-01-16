module TupleHelpers exposing (..)

import List.Extra
import Tuple


tuple3 : a -> b -> c -> (a, b, c)
tuple3 a b c =
    (a, b, c)


extractTuple3Maybe : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
extractTuple3Maybe ( ma, mb, mc ) =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


extractTuple2Maybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
extractTuple2Maybe ( ma, mb ) =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


tuple3ToList : ( a, a, a ) -> List a
tuple3ToList ( a, b, c ) =
    [ a, b, c ]


listToTuple3 : List a -> Maybe ( a, a, a )
listToTuple3 list =
    ( List.Extra.getAt 0 list
    , List.Extra.getAt 1 list
    , List.Extra.getAt 2 list
    )
        |> extractTuple3Maybe


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f =
    Tuple.mapBoth f f


tuple3First : ( a, b, c ) -> a
tuple3First ( a, b, c ) =
    a


tuple3Middle : ( a, b, c ) -> b
tuple3Middle ( a, b, c ) =
    b


tuple3Last : ( a, b, c ) -> c
tuple3Last ( a, b, c ) =
    c


mapTuple3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTuple3 f ( a, b, c ) =
    ( f a, f b, f c )


mapTuple3First : (a -> z) -> ( a, b, c ) -> ( z, b, c )
mapTuple3First f ( a, b, c ) =
    ( f a, b, c )


mapTuple3Middle : (b -> z) -> ( a, b, c ) -> ( a, z, c )
mapTuple3Middle f ( a, b, c ) =
    ( a, f b, c )


mapTuple3Last : (c -> z) -> ( a, b, c ) -> ( a, b, z )
mapTuple3Last f ( a, b, c ) =
    ( a, b, f c )


combineTuple2 : (a -> b -> c) -> ( a, b ) -> c
combineTuple2 f ( a, b ) =
    f a b


mergeTuple3 : ( a -> d -> g, b -> e -> h, c -> f -> i ) -> ( a, b, c ) -> ( d, e, f ) -> ( g, h, i )
mergeTuple3 ( aFunc, bFunc, cFunc ) ( a, b, c ) ( d, e, f ) =
    ( aFunc a d
    , bFunc b e
    , cFunc c f
    )
