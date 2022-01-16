module Demos.Landing.Reactor exposing (main)

import Demos.Landing.Main as Main


main =
    Main.reactor
        { nowInMillis = 0
        , width = 1000
        , height = 700
        }
