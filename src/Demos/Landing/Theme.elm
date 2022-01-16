module Demos.Landing.Theme exposing (..)

import Element exposing (Element, el)
import Demos.Landing.Types exposing (..)
import Element.Font as Font


normalText : String -> Element Msg
normalText s =
    el
        [ Font.color <| Element.rgb 0.8 0.8 0.8]
    <|
        Element.text s


italic : String -> Element Msg
italic =
    el [ Font.italic ] << normalText


newTabLink : { url : String, text : String } -> Element Msg
newTabLink d =
    Element.newTabLink
        []
        { url = d.url
        , label =
            Element.el
                [ Font.color <| Element.rgb 0.4 0.4 1
                ]
            <|
                Element.text d.text
        }
