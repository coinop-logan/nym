module Demos.ElementHelpers exposing (..)


type DisplayProfile
    = Desktop
    | Mobile


responsiveVal : DisplayProfile -> a -> a -> a
responsiveVal dProfile desktop mobile =
    case dProfile of
        Desktop ->
            desktop

        Mobile ->
            mobile
