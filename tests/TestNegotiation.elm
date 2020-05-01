module TestNegotiation exposing (..)

import Expect
import Localization.Negotiation as Negotiation
import Test exposing (..)


filterTests : List ( String, List { requested : List String, available : List String, defaultLocale : Maybe String, supported : List String } )
filterTests =
    [ ( "exact match"
      , [ { requested = [ "en" ], available = [ "en" ], defaultLocale = Nothing, supported = [ "en" ] }
        , { requested = [ "en-US" ], available = [ "en-US" ], defaultLocale = Nothing, supported = [ "en-US" ] }
        , { requested = [ "en-Latn-US" ], available = [ "en-Latn-US" ], defaultLocale = Nothing, supported = [ "en-Latn-US" ] }
        , { requested = [ "en-Latn-US-macos" ], available = [ "en-Latn-US-macos" ], defaultLocale = Nothing, supported = [ "en-Latn-US-macos" ] }
        , { requested = [ "rm-surmiran" ], available = [ "rm-surmiran" ], defaultLocale = Nothing, supported = [ "rm-surmiran" ] }
        , { requested = [ "de-1996" ], available = [ "de-1996" ], defaultLocale = Nothing, supported = [ "de-1996" ] }
        , { requested = [ "fr-FR" ], available = [ "de", "it", "fr-FR" ], defaultLocale = Nothing, supported = [ "fr-FR" ] }
        , { requested = [ "fr", "pl", "de-DE" ], available = [ "pl", "en-US", "de-DE" ], defaultLocale = Nothing, supported = [ "pl", "de-DE" ] }
        ]
      )
    , ( "available as range"
      , [ { requested = [ "en-US" ], available = [ "en" ], defaultLocale = Nothing, supported = [ "en" ] }
        , { requested = [ "en-Latn-US" ], available = [ "en-US" ], defaultLocale = Nothing, supported = [ "en-US" ] }
        , { requested = [ "en-US-macos" ], available = [ "en-US" ], defaultLocale = Nothing, supported = [ "en-US" ] }
        , { requested = [ "fr-CA", "de-DE" ], available = [ "fr", "it", "de" ], defaultLocale = Nothing, supported = [ "fr", "de" ] }
        , { requested = [ "ja-JP-macos" ], available = [ "ja" ], defaultLocale = Nothing, supported = [ "ja" ] }
        , { requested = [ "en-Latn-GB", "en-Latn-IN" ], available = [ "en-IN", "en-GB" ], defaultLocale = Nothing, supported = [ "en-GB", "en-IN" ] }
        ]
      )
    , ( "should match on likely subtag"
      , [ { requested = [ "en" ], available = [ "en-GB", "de", "en-US" ], defaultLocale = Nothing, supported = [ "en-US", "en-GB" ] }
        , { requested = [ "en" ], available = [ "en-Latn-GB", "de", "en-Latn-US" ], defaultLocale = Nothing, supported = [ "en-Latn-US", "en-Latn-GB" ] }
        , { requested = [ "fr" ], available = [ "fr-CA", "fr-FR" ], defaultLocale = Nothing, supported = [ "fr-FR", "fr-CA" ] }
        , { requested = [ "az-IR" ], available = [ "az-Latn", "az-Arab" ], defaultLocale = Nothing, supported = [ "az-Arab" ] }
        , { requested = [ "sr-RU" ], available = [ "sr-Cyrl", "sr-Latn" ], defaultLocale = Nothing, supported = [ "sr-Latn" ] }
        , { requested = [ "sr" ], available = [ "sr-Latn", "sr-Cyrl" ], defaultLocale = Nothing, supported = [ "sr-Cyrl" ] }
        , { requested = [ "zh-GB" ], available = [ "zh-Hans", "zh-Hant" ], defaultLocale = Nothing, supported = [ "zh-Hant" ] }
        , { requested = [ "sr", "ru" ], available = [ "sr-Latn", "ru" ], defaultLocale = Nothing, supported = [ "ru" ] }
        , { requested = [ "sr-RU" ], available = [ "sr-Latn-RO", "sr-Cyrl" ], defaultLocale = Nothing, supported = [ "sr-Latn-RO" ] }
        ]
      )
    , ( "should match cross-region"
      , [ { requested = [ "en" ], available = [ "en-US" ], defaultLocale = Nothing, supported = [ "en-US" ] }
        , { requested = [ "en-US" ], available = [ "en-GB" ], defaultLocale = Nothing, supported = [ "en-GB" ] }
        , { requested = [ "en-Latn-US" ], available = [ "en-Latn-GB" ], defaultLocale = Nothing, supported = [ "en-Latn-GB" ] }
        ]
      )
    , ( "should match cross-variant"
      , [ { requested = [ "en-US-macos" ], available = [ "en-US-windows" ], defaultLocale = Nothing, supported = [ "en-US-windows" ] }
        ]
      )
    , ( "should prioritize properly"
      , [ -- exact match first
          { requested = [ "en-US" ], available = [ "en-US-macos", "en", "en-US" ], defaultLocale = Nothing, supported = [ "en-US", "en", "en-US-macos" ] }
        , -- available as range second
          { requested = [ "en-Latn-US" ], available = [ "en-GB", "en-US" ], defaultLocale = Nothing, supported = [ "en-US", "en-GB" ] }
        , -- likely subtags third
          { requested = [ "en" ], available = [ "en-Cyrl-US", "en-Latn-US" ], defaultLocale = Nothing, supported = [ "en-Latn-US" ] }
        , -- variant range fourth
          { requested = [ "en-US-macos" ], available = [ "en-US-windows", "en-GB-macos" ], defaultLocale = Nothing, supported = [ "en-US-windows", "en-GB-macos" ] }
        , -- regional range fifth
          { requested = [ "en-US-macos" ], available = [ "en-GB-windows" ], defaultLocale = Nothing, supported = [ "en-GB-windows" ] }
        ]
      )
    , ( "should prioritize properly (extra tests)"
      , [ { requested = [ "en-US" ], available = [ "en-GB", "en" ], defaultLocale = Nothing, supported = [ "en", "en-GB" ] }
        , { requested = [ "zh-HK" ], available = [ "zh-CN", "zh-TW" ], defaultLocale = Nothing, supported = [ "zh-TW", "zh-CN" ] }
        ]
      )
    , ( "should handle default locale properly"
      , [ { requested = [ "fr" ], available = [ "de", "it" ], defaultLocale = Nothing, supported = [] }
        , { requested = [ "fr" ], available = [ "de", "it" ], defaultLocale = Just "en-US", supported = [ "en-US" ] }
        , { requested = [ "fr" ], available = [ "de", "en-US" ], defaultLocale = Just "en-US", supported = [ "en-US" ] }
        , { requested = [ "fr", "de-DE" ], available = [ "de-DE", "fr-CA" ], defaultLocale = Just "en-US", supported = [ "fr-CA", "de-DE", "en-US" ] }
        ]
      )
    , ( "should handle all matches on the 1st higher than any on the 2nd"
      , [ { requested = [ "fr-CA-macos", "de-DE" ], available = [ "de-DE", "fr-FR-windows" ], defaultLocale = Nothing, supported = [ "fr-FR-windows", "de-DE" ] }
        ]
      )
    , ( "should handle cases and underscores"
      , [ { requested = [ "fr_FR" ], available = [ "fr-FR" ], defaultLocale = Nothing, supported = [ "fr-FR" ] }
        , { requested = [ "fr_fr" ], available = [ "fr-fr" ], defaultLocale = Nothing, supported = [ "fr-fr" ] }
        , { requested = [ "fr_Fr" ], available = [ "fr-fR" ], defaultLocale = Nothing, supported = [ "fr-fR" ] }
        , { requested = [ "fr_lAtN_fr" ], available = [ "fr-Latn-FR" ], defaultLocale = Nothing, supported = [ "fr-Latn-FR" ] }
        , { requested = [ "fr_FR" ], available = [ "fr_FR" ], defaultLocale = Nothing, supported = [ "fr_FR" ] }
        , { requested = [ "fr-FR" ], available = [ "fr_FR" ], defaultLocale = Nothing, supported = [ "fr_FR" ] }
        , { requested = [ "fr_Cyrl_FR_macos" ], available = [ "fr_Cyrl_fr-macos" ], defaultLocale = Nothing, supported = [ "fr_Cyrl_fr-macos" ] }
        ]
      )
    , ( "should handle fake locales"
      , [ { requested = [ "2" ], available = [ "ąóżł" ], defaultLocale = Nothing, supported = [] }
        ]
      )
    ]


matchTests : List ( String, List { requested : List String, available : List String, defaultLocale : Maybe String, supported : List String } )
matchTests =
    [ ( "should match only one per requested"
      , [ { requested = [ "fr", "en" ], available = [ "en-US", "fr-FR", "en", "fr" ], defaultLocale = Nothing, supported = [ "fr", "en" ] } ]
      )
    ]


lookupTests : List ( String, List { requested : List String, available : List String, defaultLocale : String, supported : List String } )
lookupTests =
    [ ( "should match only one"
      , [ { requested = [ "fr-FR", "en" ], available = [ "en-US", "fr-FR", "en", "fr" ], defaultLocale = "en-US", supported = [ "fr-FR" ] } ]
      )
    ]


suite : Test
suite =
    describe "negotiation"
        [ describe "filtering" <|
            List.map
                (\( desc, tests ) ->
                    describe desc <|
                        List.map
                            (\{ requested, available, defaultLocale, supported } ->
                                test
                                    ("expect requested of "
                                        ++ String.join ", " requested
                                        ++ ", and available of "
                                        ++ String.join ", " available
                                        ++ ", and default locale of "
                                        ++ Maybe.withDefault "Nothing" defaultLocale
                                        ++ " to equal "
                                        ++ String.join ", " supported
                                    )
                                <|
                                    \_ ->
                                        Expect.equal
                                            (List.sort supported)
                                            (Negotiation.filter
                                                { requestedLocales = requested
                                                , availableLocales = available
                                                , defaultLocale = defaultLocale
                                                }
                                                |> List.sort
                                            )
                            )
                            tests
                )
                filterTests
        , describe "match" <|
            List.map
                (\( desc, tests ) ->
                    describe desc <|
                        List.map
                            (\{ requested, available, defaultLocale, supported } ->
                                test
                                    ("expect requested of "
                                        ++ String.join ", " requested
                                        ++ ", and available of "
                                        ++ String.join ", " available
                                        ++ ", and default locale of "
                                        ++ Maybe.withDefault "Nothing" defaultLocale
                                        ++ " to equal "
                                        ++ String.join ", " supported
                                    )
                                <|
                                    \_ ->
                                        Expect.equal
                                            (List.sort supported)
                                            (Negotiation.match
                                                { requestedLocales = requested
                                                , availableLocales = available
                                                , defaultLocale = defaultLocale
                                                }
                                                |> List.sort
                                            )
                            )
                            tests
                )
                matchTests
        , describe "lookup" <|
            List.map
                (\( desc, tests ) ->
                    describe desc <|
                        List.map
                            (\{ requested, available, defaultLocale, supported } ->
                                test
                                    ("expect requested of "
                                        ++ String.join ", " requested
                                        ++ ", and available of "
                                        ++ String.join ", " available
                                        ++ ", and default locale of "
                                        ++ defaultLocale
                                        ++ " to equal "
                                        ++ String.join ", " supported
                                    )
                                <|
                                    \_ ->
                                        Expect.equal
                                            (List.sort supported)
                                            (Negotiation.lookup
                                                { requestedLocales = requested
                                                , availableLocales = available
                                                , defaultLocale = defaultLocale
                                                }
                                                |> List.sort
                                            )
                            )
                            tests
                )
                lookupTests
        ]