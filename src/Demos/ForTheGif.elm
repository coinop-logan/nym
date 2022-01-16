module Demos.ForTheGif exposing (..)

import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Demos.Common
import Element exposing (Element)
import Element.Background as Background
import Html exposing (Html)
import Json.Decode as Decode
import Mouse
import Nym
import Time
import TupleHelpers
import Types exposing (NymTemplate)
import Vector2 exposing (Vector2)


morphStartInterval =
    1000


type alias Flags =
    { seed : Int }


type alias Model =
    { lookTarget : LookDirection
    , lookDirection : LookDirection
    , mainSeed : String
    , mode : Mode
    , morphCount : Int
    , clock : Float
    }


type Msg
    = NoOp
    | StartMorph
    | UpdateLookTarget Vector2
    | Animate Float


type alias LookDirection =
    Vector2


type Mode
    = StaticAndWaiting StaticState
    | Morphing MorphState


type alias StaticState =
    { nym : NymTemplate
    }


type alias MorphState =
    { oldNym : NymTemplate
    , newNym : NymTemplate
    , morphProgress : Float
    , morphVel : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        startingSeed =
            flags.seed |> String.fromInt

        nymTemplate =
            makeSpecificSeed startingSeed 0
                |> BinarySource.seedTo256Bits
                |> Nym.binarySourceToNymTemplate
                |> TupleHelpers.tuple3Last
    in
    ( { lookTarget = Vector2.zero
      , lookDirection = Vector2.zero
      , mainSeed = startingSeed
      , mode =
            StaticAndWaiting
                { nym = nymTemplate }
      , morphCount = 0
      , clock = 0
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


reactor : Flags -> Program () Model Msg
reactor flags =
    Browser.element
        { init = always <| init flags
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartMorph ->
            case model.mode of
                Morphing _ ->
                    ( model, Cmd.none )

                StaticAndWaiting staticState ->
                    let
                        morphCount =
                            if model.morphCount >= 7 then
                                0

                            else
                                model.morphCount + 1

                        newNymSource =
                            makeSpecificSeed model.mainSeed morphCount
                                |> BinarySource.seedTo256Bits
                    in
                    ( { model
                        | morphCount = morphCount
                        , mode =
                            Morphing <| startMorphState staticState newNymSource
                      }
                    , Cmd.none
                    )

        UpdateLookTarget newTarget ->
            ( { model | lookTarget = newTarget }
            , Cmd.none
            )

        Animate delta ->
            let
                mode =
                    case model.mode of
                        Morphing morphState ->
                            advanceMorphState delta morphState

                        StaticAndWaiting staticState ->
                            StaticAndWaiting staticState
            in
            ( { model
                | mode = mode
                , lookDirection =
                    Vector2.interpolate 0.1 model.lookDirection model.lookTarget
                , clock = model.clock + delta
                , lookTarget =
                    moveMouseAlongCircle model.clock
              }
            , Cmd.none
            )


startMorphState : StaticState -> BinarySource -> MorphState
startMorphState staticState source =
    { oldNym = staticState.nym
    , newNym =
        source
            |> Nym.binarySourceToNymTemplate
            |> TupleHelpers.tuple3Last
    , morphProgress = 0
    , morphVel = 0
    }


advanceMorphState : Float -> MorphState -> Mode
advanceMorphState delta morphState =
    let
        morphVel =
            morphState.morphVel + (delta / 2000)

        morphProgress =
            morphState.morphProgress + morphVel
    in
    if morphProgress >= 1 then
        StaticAndWaiting
            { nym = morphState.newNym }

    else
        Morphing
            { morphState
                | morphProgress = morphProgress
                , morphVel = morphVel
            }


moveMouseAlongCircle : Float -> Vector2
moveMouseAlongCircle clock =
    -- describe a circle that completes once every 8000 milliseconds
    let
        radians =
            (clock / 8000) * (2 * pi)

        length =
            0.1
    in
    Vector2.fromLengthAndRadians length radians
        |> Vector2.plus (Vector2 0 (length * 1.2))


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color <| Element.rgb 0.1 0.1 0.1
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 10
            ]
            [ Demos.Common.viewNymWithPixelDimensions
                ( 500, 450 )
                ( "100%", "100%" )
                model.lookDirection
                (modeToNymTemplate model.mode
                    |> Nym.renderNymTemplate False
                )
                (Decode.map (Demos.Common.mouseMoveDataToLookDir >> UpdateLookTarget) Mouse.moveDecoder)
                (Decode.map (always NoOp) (Decode.succeed ()))
            ]


modeToNymTemplate : Mode -> NymTemplate
modeToNymTemplate mode =
    case mode of
        StaticAndWaiting staticState ->
            staticState.nym

        Morphing morphState ->
            Demos.Common.interpolateNymsForRendering
                morphState.morphProgress
                morphState.oldNym
                morphState.newNym


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 (always StartMorph)
        , Browser.Events.onAnimationFrameDelta
            Animate
        ]


makeSpecificSeed : String -> Int -> String
makeSpecificSeed mainSeed morphCount =
    mainSeed ++ String.fromInt (morphCount + 1)
