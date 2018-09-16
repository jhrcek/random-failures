module Page exposing
    ( Page(..)
    , fromUrl
    , toClassFilter
    , toUrlHash
    )

import Browser.Navigation
import Color exposing (Color)
import TestFailure exposing (ClassAndMethod, StackTrace, StackTraceMode(..), TestFailure)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, s, string, top)


type Page
    = Home
    | ClassDetails String
    | MethodDetails ClassAndMethod StackTraceMode


toUrlHash : Page -> String
toUrlHash page =
    let
        pieces =
            case page of
                Home ->
                    []

                ClassDetails fqcn ->
                    [ "class", fqcn ]

                MethodDetails ( fqcn, method ) _ ->
                    [ "class", fqcn, "method", method ]
    in
    String.join "/" ("#" :: pieces)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map Home top
        , map ClassDetails (s "class" </> string)
        , map
            (\clz method -> MethodDetails ( percDecode clz, percDecode method ) NoStackTrace)
            (s "class" </> string </> s "method" </> string)
        ]


{-| Altough elm/browser docs claim thas
t string parameters are percent decoded, it's not true
-}
percDecode : String -> String
percDecode encoded =
    case Url.percentDecode encoded of
        Just decoded ->
            decoded

        Nothing ->
            encoded


fromUrl : Url -> Page
fromUrl url =
    Url.Parser.parse pageParser { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Maybe.withDefault Home


toClassFilter : Page -> Maybe String
toClassFilter page =
    case page of
        ClassDetails fqcn ->
            Just fqcn

        Home ->
            Nothing

        MethodDetails _ _ ->
            Nothing
