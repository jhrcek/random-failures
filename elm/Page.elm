module Page
    exposing
        ( ClassAndMethod
        , Page(MethodDetails, Summary)
        , parse
        , toUrlHash
        )

import Http
import Navigation
import UrlParser exposing ((</>), Parser, custom, map, oneOf, s)


type Page
    = Summary
    | MethodDetails ClassAndMethod (Maybe String)


type alias ClassAndMethod =
    ( String, String )


toUrlHash : Page -> String
toUrlHash page =
    case page of
        Summary ->
            "#/summary"

        MethodDetails ( clz, method ) _ ->
            "#/class/" ++ Http.encodeUri clz ++ "/method/" ++ Http.encodeUri method


route : Parser (Page -> a) a
route =
    oneOf
        [ map Summary (s "summary")
        , map
            (\clz method -> MethodDetails ( clz, method ) Nothing)
            (s "class" </> uriEncodedString </> s "method" </> uriEncodedString)
        ]


uriEncodedString : Parser (String -> a) a
uriEncodedString =
    custom "URI_ENCODED_STRING" <|
        \piece ->
            Result.fromMaybe ("Failed to decode URI piece" ++ piece) (Http.decodeUri piece)


parse : Navigation.Location -> Maybe Page
parse location =
    UrlParser.parseHash route location
