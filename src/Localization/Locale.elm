module Localization.Locale exposing
    ( Locale
    , addLikelySubtags
    , clearRegion
    , clearVariants
    , equal
    , fromString
    , getLanguage
    , getRegionAsString
    , getScriptAsString
    , getVariantsAsString
    , hasLanguage
    , matches
    )

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


type Locale
    = Locale InternalLocale


type alias InternalLocale =
    { language : String
    , script : Maybe String
    , region : Maybe String
    , variant : Maybe String
    }


fromString : String -> Maybe Locale
fromString str =
    String.replace "_" "-" str
        |> Parser.run parse
        |> Result.toMaybe
        |> Maybe.map Locale


toString : Locale -> String
toString (Locale { language, script, region, variant }) =
    language
        ++ localePartToString script
        ++ localePartToString region
        ++ localePartToString variant


localePartToString : Maybe String -> String
localePartToString part =
    part
        |> Maybe.map ((++) "-")
        |> Maybe.withDefault ""


hasLanguage : Locale -> Bool
hasLanguage (Locale { language }) =
    String.length language > 0


matches : { otherLocale : Locale, thisRange : Bool, otherRange : Bool } -> Locale -> Bool
matches { otherLocale, thisRange, otherRange } (Locale this) =
    let
        (Locale other) =
            otherLocale

        langCheck =
            other.language == this.language

        scriptCheck =
            case ( other.script, this.script ) of
                ( Just os, Just ts ) ->
                    os == ts

                ( Nothing, Nothing ) ->
                    True

                ( Nothing, _ ) ->
                    otherRange

                ( _, Nothing ) ->
                    thisRange

        regionCheck =
            case ( other.region, this.region ) of
                ( Just os, Just ts ) ->
                    os == ts

                ( Nothing, Nothing ) ->
                    True

                ( Nothing, _ ) ->
                    otherRange

                ( _, Nothing ) ->
                    thisRange

        variantCheck =
            case ( other.variant, this.variant ) of
                ( Just os, Just ts ) ->
                    os == ts

                ( Nothing, Nothing ) ->
                    True

                ( Nothing, _ ) ->
                    otherRange

                ( _, Nothing ) ->
                    thisRange
    in
    langCheck && scriptCheck && regionCheck && variantCheck


addLikelySubtags : Locale -> Maybe Locale
addLikelySubtags =
    toString >> getLikelySubtagsMin


clearVariants : Locale -> Locale
clearVariants (Locale locale) =
    Locale { locale | variant = Nothing }


clearRegion : Locale -> Locale
clearRegion (Locale locale) =
    Locale { locale | region = Nothing }


parse : Parser InternalLocale
parse =
    Parser.succeed InternalLocale
        |= parseLanguageCode
        |= parseScriptCode
        |= parseRegionCode
        |= parseVariantCode


{-| ([a-z]{2,3}|\\\*)
-}
parseLanguageCode : Parser String
parseLanguageCode =
    [ parseAsterisk
    , Parser.succeed ()
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
    ]
        |> Parser.oneOf
        |> Parser.andThen
            (\langCode ->
                case String.toList langCode of
                    [ '*' ] ->
                        Parser.succeed langCode

                    [ _, _ ] ->
                        Parser.succeed langCode

                    [ _, _, _ ] ->
                        Parser.succeed langCode

                    _ ->
                        Parser.problem ("Invalid language code: " ++ langCode)
            )
        |> Parser.andThen (String.toLower >> Parser.succeed)


{-| (?:-([a-z]{4}|\\\*))
-}
parseScriptCode : Parser (Maybe String)
parseScriptCode =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |= Parser.oneOf
                [ parseAsterisk
                , Parser.succeed ()
                    |. Parser.chompWhile Char.isAlpha
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (\str ->
                            if String.length str == 4 then
                                Parser.succeed str

                            else
                                Parser.problem "Script code must be exactly 4 characters long"
                        )
                ]
            |> Parser.andThen (String.toUpper >> Just >> Parser.succeed)
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


{-| (?:-([a-z]{2}|\\\*))
-}
parseRegionCode : Parser (Maybe String)
parseRegionCode =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |= Parser.oneOf
                [ parseAsterisk
                , Parser.succeed ()
                    |. Parser.chompWhile Char.isAlpha
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (\str ->
                            if String.length str == 2 then
                                Parser.succeed str

                            else
                                Parser.problem "Region code must be exactly 2 characters long"
                        )
                ]
            |> Parser.andThen (String.toUpper >> Just >> Parser.succeed)
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


{-| (?:-(([0-9][a-z0-9]{3}|[a-z0-9]{5,8})|\\\*))
-}
parseVariantCode : Parser (Maybe String)
parseVariantCode =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |= Parser.oneOf
                [ parseAsterisk
                , Parser.succeed ()
                    |. Parser.chompIf Char.isDigit
                    |. Parser.chompWhile Char.isAlphaNum
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (\str ->
                            if String.length str == 3 then
                                Parser.succeed str

                            else
                                Parser.problem "Variant code with starting digit must be exactly 3 characters long"
                        )
                , Parser.succeed ()
                    |. Parser.chompWhile Char.isAlphaNum
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (\str ->
                            let
                                len =
                                    String.length str
                            in
                            if len >= 5 && len <= 8 then
                                Parser.succeed str

                            else
                                Parser.problem "Variant code must be between 5 and 8 characters long"
                        )
                ]
            |> Parser.andThen (Just >> Parser.succeed)
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


parseAsterisk : Parser String
parseAsterisk =
    Parser.succeed ()
        |. Parser.chompIf ((==) '*')
        |> Parser.getChompedString
        |> Parser.backtrackable


{-| A manuall list of likely subtags corresponding to Unicode
CLDR likelySubtags list.
This list is curated by the maintainers of Project Fluent and is
intended to be used in place of the full likelySubtags list in use cases
where full list cannot be (for example, due to the size).
This version of the list is based on CLDR 30.0.3.

See https://github.com/projectfluent/fluent.js/blob/master/fluent-langneg/src/subtags.ts for the original reference.
-}
likelySubtagsMin : Dict String InternalLocale
likelySubtagsMin =
    Dict.fromList
        [ ( "ar", { language = "ar", script = Just "ARAB", region = Just "EG", variant = Nothing } )
        , ( "az-arab", { language = "az", script = Just "ARAB", region = Just "IR", variant = Nothing } )
        , ( "az-ir", { language = "az", script = Just "ARAB", region = Just "IR", variant = Nothing } )
        , ( "be", { language = "be", script = Just "CYRL", region = Just "BY", variant = Nothing } )
        , ( "da", { language = "da", script = Just "LATN", region = Just "DK", variant = Nothing } )
        , ( "el", { language = "el", script = Just "GREK", region = Just "GR", variant = Nothing } )
        , ( "en", { language = "en", script = Just "LATN", region = Just "US", variant = Nothing } )
        , ( "fa", { language = "fa", script = Just "ARAB", region = Just "IR", variant = Nothing } )
        , ( "ja", { language = "ja", script = Just "JPAN", region = Just "JP", variant = Nothing } )
        , ( "ko", { language = "ko", script = Just "KORE", region = Just "KR", variant = Nothing } )
        , ( "pt", { language = "pt", script = Just "LATN", region = Just "BR", variant = Nothing } )
        , ( "sr", { language = "sr", script = Just "CYRL", region = Just "RS", variant = Nothing } )
        , ( "sr-ru", { language = "sr", script = Just "LATN", region = Just "RU", variant = Nothing } )
        , ( "sv", { language = "sv", script = Just "LATN", region = Just "SE", variant = Nothing } )
        , ( "ta", { language = "ta", script = Just "TAML", region = Just "IN", variant = Nothing } )
        , ( "uk", { language = "uk", script = Just "CYRL", region = Just "UA", variant = Nothing } )
        , ( "zh", { language = "zh", script = Just "HANS", region = Just "CN", variant = Nothing } )
        , ( "zh-hant", { language = "zh", script = Just "HANT", region = Just "TW", variant = Nothing } )
        , ( "zh-hk", { language = "zh", script = Just "HANT", region = Just "HK", variant = Nothing } )
        , ( "zh-gb", { language = "zh", script = Just "HANT", region = Just "GB", variant = Nothing } )
        , ( "zh-us", { language = "zh", script = Just "HANT", region = Just "US", variant = Nothing } )
        ]


regionMatchingLangs : Set String
regionMatchingLangs =
    Set.fromList
        [ "az"
        , "bg"
        , "cs"
        , "de"
        , "es"
        , "fi"
        , "fr"
        , "hu"
        , "it"
        , "lt"
        , "lv"
        , "nl"
        , "pl"
        , "ro"
        , "ru"
        ]


getLikelySubtagsMin : String -> Maybe Locale
getLikelySubtagsMin str =
    case Dict.get (String.toLower str) likelySubtagsMin of
        Just locale ->
            Just (Locale locale)

        Nothing ->
            case fromString str of
                Nothing ->
                    Nothing

                Just (Locale locale) ->
                    if hasLanguage (Locale locale) && Set.member locale.language regionMatchingLangs then
                        Just (Locale { locale | region = Just (String.toUpper locale.language) })

                    else
                        Nothing


equal : Locale -> Locale -> Bool
equal (Locale first) (Locale second) =
    (first.language == second.language)
        && maybeEqual first.script second.script
        && maybeEqual first.region second.region
        && maybeEqual first.variant second.variant


maybeEqual : Maybe comparable -> Maybe comparable -> Bool
maybeEqual first second =
    case ( first, second ) of
        ( Just f, Just s ) ->
            f == s

        ( Nothing, Nothing ) ->
            True

        _ ->
            False


getLanguage : Locale -> String
getLanguage (Locale { language }) =
    language


getScriptAsString : Locale -> String
getScriptAsString (Locale { script }) =
    Maybe.withDefault "" script


getRegionAsString : Locale -> String
getRegionAsString (Locale { region }) =
    Maybe.withDefault "" region


getVariantsAsString : Locale -> String
getVariantsAsString (Locale { variant }) =
    Maybe.withDefault "" variant
