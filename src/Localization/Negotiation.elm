module Localization.Negotiation exposing (filter, lookup, match)

{-| Each function akes a List of Strings in the BCP47 locale ID format. If in an invalid format, they will be ignored. The list `requestedLocales` List should be in preference order The `availableLocales` is a List of Strings also in BCP47 locale ID format, unsorted. The `defaultLocale` should be a String in BCP47 locale ID format and isused as a last resort locale.


It returns an Array of strings with BCP47 locale IDs sorted according to the user preferences.

## Strategies

@docs filter
@docs lookup
@docs match
-}

import Dict exposing (Dict)
import Dict.Extra
import Localization.Locale as Locale exposing (Locale)
import Maybe.Extra
import Set


{-| Looks for the best matching available locale for each requested locale.

    Localization.Negotiation.filter
        { requestedLocales =  [ "de-DE", "fr-FR" ]
        , availableLocales = [ "it", "de", "en-US", "fr-CA", "de-DE", "fr", "de-AU" ]
        , defaultLocale = Nothing
        }

    -- supported: [ "de-DE", "fr" ]
-}
filter : { requestedLocales : List String, availableLocales : List String, defaultLocale : Maybe String } -> List String
filter { requestedLocales, availableLocales, defaultLocale } =
    addDefaultToFilteredSupported defaultLocale <|
        filterHelper
            { requestedLocales = buildRequested requestedLocales
            , availableLocales = buildAvailable availableLocales
            }

{-| Will try to match as many available locales as possible for each of the requested locale.


    Localization.Negotiation.match
        { requestedLocales =  [ "de-DE", "fr-FR" ]
        , availableLocales = [ "it", "de", "en-US", "fr-CA", "de-DE", "fr", "de-AU" ]
        , defaultLocale = Nothing
        }

    -- supported: [ "de-DE", "de", "fr", "fr-CA" ]
-}
match : { requestedLocales : List String, availableLocales : List String, defaultLocale : Maybe String } -> List String
match { requestedLocales, availableLocales, defaultLocale } =
    addDefaultToFilteredSupported defaultLocale <|
        matchHelper
            { requestedLocales = buildRequested requestedLocales
            , availableLocales = buildAvailable availableLocales
            }


addDefaultToFilteredSupported : Maybe String -> List String -> List String
addDefaultToFilteredSupported defaultLocale availableLocales =
    case defaultLocale of
        Nothing ->
            availableLocales

        Just locale ->
            if List.any ((==) locale) availableLocales then
                availableLocales

            else
                locale :: availableLocales

{-| Will try to find the single best locale for the requested locale list among the available locales. If none is found it will fallback to the `defaultLocale`.

    Localization.Negotiation.lookup
        { requestedLocales =  [ "de-DE", "fr-FR" ]
        , availableLocales = [ "it", "de", "en-US", "fr-CA", "de-DE", "fr", "de-AU" ]
        , defaultLocale = "en-US"
        }

    -- supported: [ "de-DE" ]
-}
lookup : { requestedLocales : List String, availableLocales : List String, defaultLocale : String } -> List String
lookup { requestedLocales, availableLocales, defaultLocale } =
    let
        supportedFiltered =
            lookupHelper
                { requestedLocales = buildLookupRequested requestedLocales
                , availableLocales = buildAvailable availableLocales
                }
    in
    if List.isEmpty supportedFiltered then
        [ defaultLocale ]

    else
        supportedFiltered


buildLookupRequested : List String -> List ( String, Locale )
buildLookupRequested =
    List.foldr
        (\str req ->
            case Locale.fromString str of
                Nothing ->
                    req

                Just locale ->
                    if Locale.hasLanguage locale then
                        ( str, locale ) :: req

                    else
                        req
        )
        []

{-| This and the other helpers are based off https://github.com/projectfluent/fluent.js/blob/master/fluent-langneg/src/matches.ts.

They have been modified to fit the Elm language as well as to take advantage of pure, immutable functions.

Copied full text from the source:

Negotiates the languages between the list of requested locales against
a list of available locales.

The algorithm is based on the BCP4647 3.3.2 Extended Filtering algorithm,
with several modifications:

 1) available locales are treated as ranges

   This change allows us to match a more specific request against
   more generic available locale.

   For example, if the available locale list provides locale `en`,
   and the requested locale is `en-US`, we treat the available locale as
   a locale that matches all possible english requests.

   This means that we expect available locale ID to be as precize as
   the matches they want to cover.

   For example, if there is only `sr` available, it's ok to list
   it in available locales. But once the available locales has both,
   Cyrl and Latn variants, the locale IDs should be `sr-Cyrl` and `sr-Latn`
   to avoid any `sr-*` request to match against whole `sr` range.

   What it does ([requested] * [available] = [supported]):

   ['en-US'] * ['en'] = ['en']

 2) likely subtags from LDML 4.3 Likely Subtags has been added

   The most obvious likely subtag that can be computed is a duplication
   of the language field onto region field (`fr` => `fr-FR`).

   On top of that, likely subtags may use a list of mappings, that
   allow the algorithm to handle non-obvious matches.
   For example, making sure that we match `en` to `en-US` or `sr` to
   `sr-Cyrl`, while `sr-RU` to `sr-Latn-RU`.

   This list can be taken directly from CLDR Supplemental Data.

   What it does ([requested] * [available] = [supported]):

   ['fr'] * ['fr-FR'] = ['fr-FR']
   ['en'] * ['en-US'] = ['en-US']
   ['sr'] * ['sr-Latn', 'sr-Cyrl'] = ['sr-Cyrl']

 3) variant/region range check has been added

   Lastly, the last form of check is against the requested locale ID
   but with the variant/region field replaced with a `*` range.

   The rationale here laid out in LDML 4.4 Language Matching:
     "(...) normally the fall-off between the user's languages is
     substantially greated than regional variants."

   In other words, if we can't match for the given region, maybe
   we can match for the same language/script but other region, and
   it will in most cases be preferred over falling back on the next
   language.

   What it does ([requested] * [available] = [supported]):

   ['en-AU'] * ['en-US'] = ['en-US']
   ['sr-RU'] * ['sr-Latn-RO'] = ['sr-Latn-RO'] // sr-RU -> sr-Latn-RU

   It works similarly to getParentLocales algo, except that we stop
   after matching against variant/region ranges and don't try to match
   ignoring script ranges. That means that `sr-Cyrl` will never match
   against `sr-Latn`.
-}
matchHelper : { requestedLocales : Dict String Locale, availableLocales : Dict String Locale } -> List String
matchHelper { requestedLocales, availableLocales } =
    [ -- 1) Attempt to make an exact match
      -- Example: `en-US` === `en-US`
      localeFindHelper
        availableLocales
        (\reqKey _ availKey _ -> String.toLower reqKey == String.toLower availKey)
        requestedLocales

    -- 2) Attempt to match against the available range
    -- This turns `en` into `en-*-*-*` and `en-US` into `en-*-US-*`
    -- Example: ['en-US'] * ['en'] = ['en']
    , localeFindHelper
        availableLocales
        (\_ reqLocale _ availLocale ->
            Locale.matches { otherLocale = reqLocale, otherRange = False, thisRange = True } availLocale
        )
        requestedLocales

    -- 3) Attempt to retrieve a maximal version of the requested locale ID
    -- If data is available, it'll expand `en` into `en-Latn-US` and
    -- `zh` into `zh-Hans-CN`.
    -- Example: ['en'] * ['en-GB', 'en-US'] = ['en-US']
    , requestedLocales
        |> Dict.foldl
            (\reqKey reqLocale found ->
                Maybe.Extra.orLazy found <|
                    \() ->
                        case Locale.addLikelySubtags reqLocale of
                            Nothing ->
                                found

                            Just rLoc ->
                                Dict.Extra.find
                                    (\availKey availLocale ->
                                        Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                                    )
                                    availableLocales
            )
            Nothing

    -- 4) Attempt to look up for a different variant for the same locale ID
    -- Example: ['en-US-mac'] * ['en-US-win'] = ['en-US-win']
    , localeFindHelper
        availableLocales
        (\_ reqLocale availKey availLocale ->
            let
                rLoc =
                    reqLocale
                        |> Locale.addLikelySubtags
                        |> Maybe.withDefault reqLocale
                        |> Locale.clearVariants
            in
            Locale.matches { otherLocale = rLoc, otherRange = True, thisRange = True } availLocale
        )
        requestedLocales

    -- 5) Attempt to match against the likely subtag without region
    -- In the example below, addLikelySubtags will turn
    -- `zh-Hant` into `zh-Hant-TW` giving `zh-TW` priority match
    -- over `zh-CN`.
    --
    -- Example: ['zh-Hant-HK'] * ['zh-TW', 'zh-CN'] = ['zh-TW']
    , requestedLocales
        |> Dict.foldl
            (\reqKey reqLocale found ->
                Maybe.Extra.orLazy found <|
                    \() ->
                        let
                            subTag =
                                reqLocale
                                    |> Locale.addLikelySubtags
                                    |> Maybe.withDefault reqLocale
                                    |> Locale.clearVariants
                                    |> Locale.clearRegion
                                    |> Locale.addLikelySubtags
                        in
                        case subTag of
                            Nothing ->
                                found

                            Just rLoc ->
                                Dict.Extra.find
                                    (\availKey availLocale ->
                                        Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                                    )
                                    availableLocales
            )
            Nothing

    -- 6) Attempt to look up for a different region for the same locale ID
    -- Example: ['en-US'] * ['en-AU'] = ['en-AU']
    , requestedLocales
        |> Dict.foldl
            (\reqKey reqLocale found ->
                Maybe.Extra.orLazy found <|
                    \() ->
                        let
                            rLocFrom5 =
                                reqLocale
                                    |> Locale.addLikelySubtags
                                    |> Maybe.withDefault reqLocale
                                    |> Locale.clearVariants
                                    |> Locale.clearRegion

                            rLoc =
                                rLocFrom5
                                    |> Locale.addLikelySubtags
                                    |> Maybe.withDefault rLocFrom5
                                    |> Locale.clearRegion
                        in
                        Dict.Extra.find
                            (\availKey availLocale ->
                                Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                            )
                            availableLocales
            )
            Nothing
    ]
        |> List.filterMap (Maybe.map Tuple.first)
        |> Set.fromList
        |> Set.toList


lookupHelper : { requestedLocales : List ( String, Locale ), availableLocales : Dict String Locale } -> List String
lookupHelper { requestedLocales, availableLocales } =
    [ -- 1) Attempt to make an exact match
      -- Example: `en-US` === `en-US`
      \() ->
        lookupLocaleFindHelper
            availableLocales
            (\( reqKey, _ ) availKey _ -> String.toLower reqKey == String.toLower availKey)
            requestedLocales

    -- 2) Attempt to match against the available range
    -- This turns `en` into `en-*-*-*` and `en-US` into `en-*-US-*`
    -- Example: ['en-US'] * ['en'] = ['en']
    , \() ->
        lookupLocaleFindHelper
            availableLocales
            (\( _, reqLocale ) _ availLocale ->
                Locale.matches { otherLocale = reqLocale, otherRange = False, thisRange = True } availLocale
            )
            requestedLocales

    -- 3) Attempt to retrieve a maximal version of the requested locale ID
    -- If data is available, it'll expand `en` into `en-Latn-US` and
    -- `zh` into `zh-Hans-CN`.
    -- Example: ['en'] * ['en-GB', 'en-US'] = ['en-US']
    , \() ->
        requestedLocales
            |> List.foldl
                (\( reqKey, reqLocale ) found ->
                    Maybe.Extra.orLazy found <|
                        \() ->
                            case Locale.addLikelySubtags reqLocale of
                                Nothing ->
                                    found

                                Just rLoc ->
                                    Dict.Extra.find
                                        (\availKey availLocale ->
                                            Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                                        )
                                        availableLocales
                )
                Nothing

    -- 4) Attempt to look up for a different variant for the same locale ID
    -- Example: ['en-US-mac'] * ['en-US-win'] = ['en-US-win']
    , \() ->
        lookupLocaleFindHelper
            availableLocales
            (\( _, reqLocale ) availKey availLocale ->
                let
                    rLoc =
                        reqLocale
                            |> Locale.addLikelySubtags
                            |> Maybe.withDefault reqLocale
                            |> Locale.clearVariants
                in
                Locale.matches { otherLocale = rLoc, otherRange = True, thisRange = True } availLocale
            )
            requestedLocales

    -- 5) Attempt to match against the likely subtag without region
    -- In the example below, addLikelySubtags will turn
    -- `zh-Hant` into `zh-Hant-TW` giving `zh-TW` priority match
    -- over `zh-CN`.
    --
    -- Example: ['zh-Hant-HK'] * ['zh-TW', 'zh-CN'] = ['zh-TW']
    , \() ->
        requestedLocales
            |> List.foldl
                (\( reqKey, reqLocale ) found ->
                    Maybe.Extra.orLazy found <|
                        \() ->
                            let
                                subTag =
                                    reqLocale
                                        |> Locale.addLikelySubtags
                                        |> Maybe.withDefault reqLocale
                                        |> Locale.clearVariants
                                        |> Locale.clearRegion
                                        |> Locale.addLikelySubtags
                            in
                            case subTag of
                                Nothing ->
                                    found

                                Just rLoc ->
                                    Dict.Extra.find
                                        (\availKey availLocale ->
                                            Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                                        )
                                        availableLocales
                )
                Nothing

    -- 6) Attempt to look up for a different region for the same locale ID
    -- Example: ['en-US'] * ['en-AU'] = ['en-AU']
    , \() ->
        requestedLocales
            |> List.foldl
                (\( reqKey, reqLocale ) found ->
                    Maybe.Extra.orLazy found <|
                        \() ->
                            let
                                rLocFrom5 =
                                    reqLocale
                                        |> Locale.addLikelySubtags
                                        |> Maybe.withDefault reqLocale
                                        |> Locale.clearVariants
                                        |> Locale.clearRegion

                                rLoc =
                                    rLocFrom5
                                        |> Locale.addLikelySubtags
                                        |> Maybe.withDefault rLocFrom5
                                        |> Locale.clearRegion
                            in
                            Dict.Extra.find
                                (\availKey availLocale ->
                                    Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale
                                )
                                availableLocales
                )
                Nothing
    ]
        |> Maybe.Extra.orListLazy
        |> Maybe.map (Tuple.first >> List.singleton)
        |> Maybe.withDefault []


localeFindHelper : Dict String Locale -> (String -> Locale -> String -> Locale -> Bool) -> Dict String Locale -> Maybe ( String, Locale )
localeFindHelper availableLocales findFunc =
    dictFindValue
        (\reqKey reqLocale ->
            Dict.Extra.find (findFunc reqKey reqLocale) availableLocales
        )


lookupLocaleFindHelper : Dict String Locale -> (( String, Locale ) -> String -> Locale -> Bool) -> List ( String, Locale ) -> Maybe ( String, Locale )
lookupLocaleFindHelper availableLocales findFunc =
    fauxDict
        (\( reqKey, reqLocale ) ->
            Dict.Extra.find (findFunc ( reqKey, reqLocale )) availableLocales
        )


fauxDict : (( comparable, a ) -> Maybe b) -> List ( comparable, a ) -> Maybe b
fauxDict pred dict =
    fauxDictHelper pred dict


fauxDictHelper : (( comparable, a ) -> Maybe b) -> List ( comparable, a ) -> Maybe b
fauxDictHelper pred dict =
    case dict of
        [] ->
            Nothing

        ( key, val ) :: rest ->
            Maybe.Extra.orLazy
                (pred ( key, val ))
                (\() -> fauxDictHelper pred rest)


filterHelper : { requestedLocales : Dict String Locale, availableLocales : Dict String Locale } -> List String
filterHelper { requestedLocales, availableLocales } =
    requestedLocales
        |> Dict.foldl
            (\reqKey reqLocale ( supported, remainingAvailable ) ->
                let
                    -- 1) Attempt to make an exact match
                    -- Example: `en-US` === `en-US`
                    ( support1, available1 ) =
                        Dict.foldl
                            (\availKey availLocale ( sup, rem ) ->
                                if String.toLower reqKey == String.toLower availKey then
                                    ( Set.insert availKey sup, Dict.remove availKey rem )

                                else
                                    ( sup, rem )
                            )
                            ( supported, remainingAvailable )
                            remainingAvailable

                    -- 2) Attempt to match against the available range
                    -- This turns `en` into `en-*-*-*` and `en-US` into `en-*-US-*`
                    -- Example: ['en-US'] * ['en'] = ['en']
                    ( support2, available2 ) =
                        Dict.foldl
                            (\availKey availLocale ( sup, rem ) ->
                                if Locale.matches { otherLocale = reqLocale, otherRange = False, thisRange = True } availLocale then
                                    ( Set.insert availKey sup, Dict.remove availKey rem )

                                else
                                    ( sup, rem )
                            )
                            ( support1, available1 )
                            available1

                    -- 3) Attempt to retrieve a maximal version of the requested locale ID
                    -- If data is available, it'll expand `en` into `en-Latn-US` and
                    -- `zh` into `zh-Hans-CN`.
                    -- Example: ['en'] * ['en-GB', 'en-US'] = ['en-US']
                    ( support3, available3 ) =
                        case Locale.addLikelySubtags reqLocale of
                            Nothing ->
                                ( support2, available2 )

                            Just rLoc ->
                                Dict.foldl
                                    (\availKey availLocale ( sup, rem ) ->
                                        if Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale then
                                            ( Set.insert availKey sup, Dict.remove availKey rem )

                                        else
                                            ( sup, rem )
                                    )
                                    ( support2, available2 )
                                    available2

                    -- 4) Attempt to look up for a different variant for the same locale ID
                    -- Example: ['en-US-mac'] * ['en-US-win'] = ['en-US-win']
                    ( support4, available4 ) =
                        let
                            rLoc =
                                case Locale.addLikelySubtags reqLocale of
                                    Nothing ->
                                        reqLocale

                                    Just r ->
                                        r
                        in
                        Dict.foldl
                            (\availKey availLocale ( sup, rem ) ->
                                if Locale.matches { otherLocale = Locale.clearVariants rLoc, otherRange = True, thisRange = True } availLocale then
                                    ( Set.insert availKey sup, Dict.remove availKey rem )

                                else
                                    ( sup, rem )
                            )
                            ( support3, available3 )
                            available3

                    -- 5) Attempt to match against the likely subtag without region
                    -- In the example below, addLikelySubtags will turn
                    -- `zh-Hant` into `zh-Hant-TW` giving `zh-TW` priority match
                    -- over `zh-CN`.
                    --
                    -- Example: ['zh-Hant-HK'] * ['zh-TW', 'zh-CN'] = ['zh-TW']
                    ( support5, available5 ) =
                        let
                            subTag =
                                reqLocale
                                    |> Locale.addLikelySubtags
                                    |> Maybe.withDefault reqLocale
                                    |> Locale.clearVariants
                                    |> Locale.clearRegion
                                    |> Locale.addLikelySubtags
                        in
                        case subTag of
                            Nothing ->
                                ( support4, available4 )

                            Just rLoc ->
                                Dict.foldl
                                    (\availKey availLocale ( sup, rem ) ->
                                        if Locale.matches { otherLocale = rLoc, otherRange = False, thisRange = True } availLocale then
                                            ( Set.insert availKey sup, Dict.remove availKey rem )

                                        else
                                            ( sup, rem )
                                    )
                                    ( support4, available4 )
                                    available4

                    rLocFrom5 =
                        reqLocale
                            |> Locale.addLikelySubtags
                            |> Maybe.withDefault reqLocale
                            |> Locale.clearVariants
                            |> Locale.clearRegion

                    rLocFor6 =
                        rLocFrom5
                            |> Locale.addLikelySubtags
                            |> Maybe.withDefault rLocFrom5
                            |> Locale.clearRegion
                in
                -- 6) Attempt to look up for a different region for the same locale ID
                -- Example: ['en-US'] * ['en-AU'] = ['en-AU']
                Dict.foldl
                    (\availKey availLocale ( sup, rem ) ->
                        if Locale.matches { otherLocale = rLocFor6, otherRange = True, thisRange = True } availLocale then
                            ( Set.insert availKey sup, Dict.remove availKey rem )

                        else
                            ( sup, rem )
                    )
                    ( support5, available5 )
                    available5
            )
            ( Set.empty, availableLocales )
        |> Tuple.first
        |> Set.toList


buildAvailable : List String -> Dict String Locale
buildAvailable =
    Set.fromList
        >> Set.foldl
            (\str avail ->
                case Locale.fromString str of
                    Nothing ->
                        avail

                    Just locale ->
                        Dict.insert str locale avail
            )
            Dict.empty


buildRequested : List String -> Dict String Locale
buildRequested =
    Set.fromList
        >> Set.foldl
            (\str req ->
                case Locale.fromString str of
                    Nothing ->
                        req

                    Just locale ->
                        if Locale.hasLanguage locale then
                            Dict.insert str locale req

                        else
                            req
            )
            Dict.empty


dictFindValue : (comparable -> a -> Maybe b) -> Dict comparable a -> Maybe b
dictFindValue pred dict =
    dictFindValueHelper pred (Dict.toList dict)


dictFindValueHelper : (comparable -> a -> Maybe b) -> List ( comparable, a ) -> Maybe b
dictFindValueHelper pred dict =
    case dict of
        [] ->
            Nothing

        ( key, val ) :: rest ->
            Maybe.Extra.orLazy
                (pred key val)
                (\() -> dictFindValueHelper pred rest)
