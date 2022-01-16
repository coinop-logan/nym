module Demos.Morph exposing (..)

import BinarySource exposing (BinarySource)
import Browser
import Browser.Events
import Crypto.Hash as Hash
import Demos.Common
import Element exposing (Element)
import Html exposing (Html)
import Json.Decode as Decode
import LineSegment3d exposing (LineSegment3d, endPoint)
import Mouse
import Nym exposing (..)
import Point2d exposing (Point2d)
import Scene3d.Material as Material exposing (Material)
import Time
import TupleHelpers
import Types exposing (..)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


type Msg
    = MouseMove Mouse.MoveData
    | NewSeed
    | AnimateDelta Float
    | MaybeChangeLookDir Time.Posix
    | MaybeChangeSeed Time.Posix
    | UpdateNow Time.Posix
    | NoOp


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput
    , laggedMouse : MouseInput
    , oldNymTemplate : NymTemplate
    , newNymTemplate : NymTemplate
    , morphProgress : Float
    , morphAccel : Float
    , lastGeneratedSource : BinarySource
    , lastMouseMoveTime : Time.Posix
    , lastMouseClickTime : Time.Posix
    , now : Time.Posix
    }


initModel : String -> Model
initModel seed =
    let
        source =
            seed
                |> BinarySource.seedTo256Bits

        nymTemplate =
            source
                |> Demos.Common.genNymTemplate
    in
    { mouseInput = MouseInput 0 0
    , laggedMouse = MouseInput 0 0
    , oldNymTemplate = nymTemplate
    , newNymTemplate = nymTemplate
    , morphProgress = 1
    , morphAccel = 0
    , lastGeneratedSource = source
    , lastMouseMoveTime = Time.millisToPosix 0
    , lastMouseClickTime = Time.millisToPosix 0
    , now = Time.millisToPosix 0
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( initModel ""
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNow time ->
            ( { model | now = time }
            , Cmd.none
            )

        MouseMove moveData ->
            ( { model
                | mouseInput =
                    Demos.Common.mouseMoveDataToLookDir moveData
                , lastMouseMoveTime = model.now
              }
            , Cmd.none
            )

        NewSeed ->
            ( { model
                | lastMouseClickTime = model.now
              }
                |> updateWithNewSource
            , Cmd.none
            )

        AnimateDelta delta ->
            let
                mouseInterpConstant =
                    if mouseMoveIsIdle model then
                        0.01

                    else
                        0.1

                morphRateConstant =
                    if mouseClickIsIdle model then
                        5000

                    else
                        5000

                morphAccel =
                    model.morphAccel + (delta / morphRateConstant)
            in
            ( { model
                | morphAccel = morphAccel
                , morphProgress =
                    min
                        1
                        (model.morphProgress + morphAccel)
                , laggedMouse =
                    Vector2.interpolate mouseInterpConstant model.laggedMouse model.mouseInput
              }
            , Cmd.none
            )

        MaybeChangeLookDir time ->
            let
                maybeNewInputAndSource =
                    if not <| mouseMoveIsIdle model then
                        Nothing

                    else
                        model.lastGeneratedSource
                            |> BinarySource.cycleWithSalt (time |> Time.posixToMillis |> String.fromInt)
                            |> BinarySource.consume2
                                ( BinarySource.consumeBool
                                , BinarySource.consume2
                                    ( BinarySource.consumeFloatRange 3 ( -0.3, 0.3 )
                                    , BinarySource.consumeFloatRange 3 ( -0.2, 0.4 )
                                    )
                                )
                            |> Maybe.andThen
                                (\( newSource, ( shouldChange, newDir ), _ ) ->
                                    if shouldChange then
                                        Nothing

                                    else
                                        Just <| ( newSource, Vector2.fromTuple newDir )
                                )

                newModel =
                    case maybeNewInputAndSource of
                        Just ( newSource, newInput ) ->
                            { model
                                | mouseInput = newInput
                                , lastGeneratedSource = newSource
                            }

                        Nothing ->
                            model
            in
            ( newModel
            , Cmd.none
            )

        MaybeChangeSeed time ->
            let
                doChangeSeed =
                    if not <| mouseClickIsIdle model then
                        False

                    else
                        model.lastGeneratedSource
                            |> BinarySource.cycleWithSalt (String.fromInt <| Time.posixToMillis time)
                            |> BinarySource.consumeInt 2
                            |> Maybe.map TupleHelpers.tuple3Middle
                            |> Maybe.withDefault 0
                            |> (==) 0

                newModel =
                    if doChangeSeed then
                        model |> updateWithNewSource

                    else
                        model
            in
            ( newModel
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


mouseMoveIsIdle : Model -> Bool
mouseMoveIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseMoveTime > 2


mouseClickIsIdle : Model -> Bool
mouseClickIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseClickTime > 4


updateWithNewSource : Model -> Model
updateWithNewSource model =
    let
        newSource =
            model.lastGeneratedSource
                |> BinarySource.cycle
    in
    { model
        | lastGeneratedSource = newSource
        , oldNymTemplate =
            Demos.Common.interpolateNymsForRendering
                model.morphProgress
                model.oldNymTemplate
                model.newNymTemplate
        , newNymTemplate = Demos.Common.genNymTemplate newSource
        , morphProgress = 0
        , morphAccel = 0
    }


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
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
                model.laggedMouse
                (Demos.Common.interpolateNymsForRendering
                    model.morphProgress
                    model.oldNymTemplate
                    model.newNymTemplate
                    |> renderNymTemplate False
                )
                (Decode.map MouseMove Mouse.moveDecoder)
                (Decode.map (always NewSeed) (Decode.succeed ()))
            ]


type UserCmd
    = NewRandomSeed


interpetCmd : String -> Maybe UserCmd
interpetCmd s =
    if s == " " then
        Just NewRandomSeed

    else
        Nothing


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta
            AnimateDelta
        , Time.every 1000
            MaybeChangeLookDir
        , Time.every 1200
            MaybeChangeSeed
        , Time.every 500
            UpdateNow
        ]
