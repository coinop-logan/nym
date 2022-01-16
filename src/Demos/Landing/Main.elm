module Demos.Landing.Main exposing (main, reactor)

import Browser
import Demos.Landing.State as State
import Demos.Landing.Types exposing (..)
import Demos.Landing.View as View


main : Program Flags Model Msg
main =
    Browser.element
        { init = State.init
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }


reactor : Flags -> Program () Model Msg
reactor flags =
    Browser.element
        { init = always <| State.init flags
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
