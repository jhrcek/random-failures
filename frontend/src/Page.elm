module Page exposing
    ( Page(..)
    , fromUrl
    , toClassFilter
    , toUrlHash
    )

import TestFailure exposing (ClassAndMethod, StackTraceMode(..))
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


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
                    [ "class", Url.percentEncode fqcn ]

                MethodDetails ( fqcn, method ) _ ->
                    [ "class", Url.percentEncode fqcn, "method", Url.percentEncode method ]
    in
    String.join "/" ("#" :: pieces)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map Home top
        , map (\fqcn -> ClassDetails (percDecode fqcn))
            (s "class" </> string)
        , map (\fqcn method -> MethodDetails ( percDecode fqcn, percDecode method ) NoStackTrace)
            (s "class" </> string </> s "method" </> string)
        ]


{-| Although elm/browser docs claim that
string parameters are percent decoded, it's not true
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
