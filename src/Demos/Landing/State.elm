module Demos.Landing.State exposing (..)

import Browser.Events
import Demos.Common
import Demos.Landing.Config as Config
import Demos.Landing.Types exposing (..)
import Demos.Morph
import List.Extra


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { morphModels =
            List.Extra.initialize
                Config.numMorphModels
                (\i ->
                    Demos.Morph.initModel (String.fromInt <| flags.nowInMillis + i)
                )
      , dProfile = Demos.Common.screenWidthToDisplayProfile flags.width
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )
        
        Resize width _ ->
            ( { model
                | dProfile =
                    Demos.Common.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        MorphMsg which morphMsg ->
            case model.morphModels |> List.Extra.getAt which of
                Just morphModel ->
                    let
                        ( newMorphModel, morphCmd ) =
                            Demos.Morph.update morphMsg morphModel
                    in
                    ( { model
                        | morphModels =
                            model.morphModels
                                |> List.Extra.setAt which newMorphModel
                      }
                    , Cmd.map (MorphMsg which) morphCmd
                    )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    ((model.morphModels
        |> List.indexedMap
            (\i morphModel ->
                Demos.Morph.subscriptions morphModel
                    |> Sub.map (MorphMsg i)
            )
     )
        ++ [ Browser.Events.onResize Resize ]
    )
        |> Sub.batch
