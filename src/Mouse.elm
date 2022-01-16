module Mouse exposing (..)

import Json.Decode as Decode exposing (Decoder)

type alias MoveData =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


moveDecoder : Decoder MoveData
moveDecoder =
    Decode.map4 MoveData
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        (Decode.at [ "target", "offsetWidth" ] Decode.float)
