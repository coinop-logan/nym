module Demos.Landing.View exposing (..)

import Demos.Common
import Demos.ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Demos.Landing.Theme exposing (italic, newTabLink, normalText)
import Demos.Landing.Types exposing (..)
import Demos.Morph
import Element exposing (Attribute, Element, centerX, column, el, fill, padding, paddingEach, paragraph, px, row, shrink, spacing, wrappedRow)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import Mouse
import Nym
import Vector2 exposing (Vector2)


view : Model -> Html Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    Element.layout
        [ Element.width fill
        , Element.height Element.shrink
        , Background.color <| Element.rgb 0.1 0.1 0.1
        , Element.paddingXY 0 50
        ]
    <|
        el
            (responsiveVal dProfile
                [ paddingEach
                    { top = 50
                    , bottom = 0
                    , right = 20
                    , left = 20
                    }
                , Element.width <| (fill |> Element.maximum 1000)
                , Element.centerX
                ]
                [ padding 30
                , Element.width fill
                ]
            )
        <|
            body dProfile model


body : DisplayProfile -> Model -> Element Msg
body dProfile model =
    column
        [ Element.width fill
        , spacing <| responsiveVal dProfile 25 40
        , Font.size <| responsiveVal dProfile 20 40
        ]
        [ paragraphs dProfile
            [ [ normalText "Nyms combine two trends in the NFT space: PFP (profile pic) projects like "
              , newTabLink
                    { url = "https://boredapeyachtclub.com/"
                    , text = "Bored Apes"
                    }
              , normalText " and generative NFTs like "
              , newTabLink
                    { url = "https://tylerxhobbs.com/fidenza"
                    , text = "Fidenza"
                    }
              , normalText ". As a PFP project, Nyms are intended to be used as unique a social avatar, easily recognizable as an "
              , italic "individual"
              , normalText " on various social platforms. But as a generative NFT project, each Nym has a uniqueness that goes far above the typical PFP project's method of simply layering a limited set features onto a template."
              ]
            , [ normalText "Instead, each Nym's structuring and color is generated bottom-up from a random data source, with constraints designed to target a mammal-like head while leaving a lot of room for variety. This results in more striking visual distinction between the items in the set, and drastically increases the possibility space of the set."
              ]
            , [ normalText "We call this first set Alpha Nyms. These consume 113 bits of entropy (72 for structure and 41 for color), resulting in over one thousand quintillion (1,000,000,000,000,000,000,000,000,000,000,000) visually distinct possibilities. Here are some of them, morphing through various possibilities:"
              ]
            ]
        , el
            [ paddingEach
                { bottom = 40
                , top = 0
                , right = 0
                , left = 0
                }
            , centerX
            ]
          <|
            viewMorphDemos model.morphModels
        , paragraphs dProfile
            [ [ normalText "Nyms exhibit \"emergent rarity\": we expect some Nyms to be much more highly valued than others, not because this rarity was declared by protocol, but because a few of the Nyms will \"emerge\" looking a little like (for instance) the Metamask fox." ]
            , [ normalText "One Alpha Nym each was airdropped on 2021.12.16 to any address that ever held FRY, the token for "
              , newTabLink
                    { url = "https://foundrydao.com/"
                    , text = "the Foundry project"
                    }
              , normalText ", as of Ethereum block 13727946 (2021.12.2); see the "
              , newTabLink
                    { url = "https://opensea.io/collection/alpha-nyms/"
                    , text = "OpenSea collection"
                    }
              , normalText ". This set, as with all future Nym sets, are generated randomly (technically, using block headers as an input). Combine this with the huge possibility space of the set, and you can be sure that no one will ever get a Nym that looks quite like another."
              ]
            , [ normalText "If this concept is validated by hype, we will move forward to produce a Beta Nym set, complete with a never-ending but rate-limited sale of a much more visually polished execution of the core Nym idea. A goal for this version is to consume a full 256 bits of entropy into visual features - this would make Nyms as uncollidable and \"effectively infinite\" as hashes."
              ]
            , [ normalText "We will be updating this page regularly after the airdrop, with more info on our plans and how we envision Nyms being used. In the meantime, come say hi on "
              , newTabLink
                    { url = "https://discord.gg/35eMjwmKXU"
                    , text = "Discord"
                    }
              , normalText "!"
              ]
            ]
        ]


paragraphs : DisplayProfile -> List (List (Element Msg)) -> Element Msg
paragraphs dProfile items =
    Element.column
        [ spacing <| responsiveVal dProfile 15 30
        , Element.width fill
        ]
        (List.map
            (paragraph
                [ spacing 2
                , Element.width fill
                ]
            )
            items
        )


viewMorphDemos : List Demos.Morph.Model -> Element Msg
viewMorphDemos morphModels =
    wrappedRow
        [ Element.spaceEvenly
        , Element.width <| px 600
        , Element.centerX
        ]
        (morphModels
            |> List.indexedMap
                (\i morphModel ->
                    el
                        [ Element.width <| px 200
                        , Element.height <| px 200
                        ]
                    <|
                        Element.map (MorphMsg i) <|
                            viewMorphDemo morphModel
                )
        )


viewMorphDemo : Demos.Morph.Model -> Element Demos.Morph.Msg
viewMorphDemo morphModel =
    el
        [ Element.width <| px 300
        , Element.centerX
        ]
    <|
        Demos.Common.viewNymWithPixelDimensions
            ( 300, 300 )
            ( "100%", "100%" )
            morphModel.laggedMouse
            (Demos.Common.interpolateNymsForRendering
                morphModel.morphProgress
                morphModel.oldNymTemplate
                morphModel.newNymTemplate
                |> Nym.renderNymTemplate False
            )
            (Decode.map Demos.Morph.MouseMove Mouse.moveDecoder)
            (Decode.map (always Demos.Morph.NewSeed) (Decode.succeed ()))
