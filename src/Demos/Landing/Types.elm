module Demos.Landing.Types exposing (..)

import Demos.ElementHelpers exposing (DisplayProfile)
import Demos.Morph
import Types exposing (..)
import Vector2 exposing (Vector2)


type alias Flags =
    { nowInMillis : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { morphModels : List Demos.Morph.Model
    , dProfile : DisplayProfile
    }


type Msg
    = NoOp
    | Resize Int Int
    | MorphMsg Int Demos.Morph.Msg
