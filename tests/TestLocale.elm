module TestLocale exposing (..)

import Expect
import Localization.Locale as Locale
import Test exposing (..)


suite : Test
suite =
    describe "simple"
        [ test "2 char language" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en"
        , test "3 char language" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "lij"
        , test "language and script" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en-Latn"
        , test "language, script, and region" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en-Latn-US"
        , test "language, script, region, and variant" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "lij-Arab-FA-linux"
        , test "language and region" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en-US"
        , test "language, region, and variant" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "lij-FA-linux"
        , describe "language range"
            [ test "language only" <|
                \_ ->
                    Expect.notEqual Nothing <|
                        Locale.fromString "*"
            , test "script" <|
                \_ ->
                    Expect.notEqual Nothing <|
                        Locale.fromString "*-Latn"
            , test "region" <|
                \_ ->
                    Expect.notEqual Nothing <|
                        Locale.fromString "*-US"
            ]
        , describe "script range"
            [ test "language" <|
                \_ ->
                    Expect.notEqual Nothing <|
                        Locale.fromString "en-*"
            , test "language and region" <|
                \_ ->
                    Expect.notEqual Nothing <|
                        Locale.fromString "en-*-US"
            ]
        , test "region range" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en-Latn-*"
        , test "variant range" <|
            \_ ->
                Expect.notEqual Nothing <|
                    Locale.fromString "en-Latn-US-*"
        ]