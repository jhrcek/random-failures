module Page
    exposing
        ( ClassAndMethod
        , Page(MethodDetails, Summary)
        , parse
        , toUrlHash
        )

import Navigation
import UrlParser exposing ((</>), Parser, map, oneOf, s, string)


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
            "#/class/" ++ clz ++ "/method/" ++ method


route : Parser (Page -> a) a
route =
    oneOf
        [ map Summary (s "summary")
        , map
            (\clz method -> MethodDetails ( clz, method ) Nothing)
            (s "class" </> string </> s "method" </> string)
        ]


parse : Navigation.Location -> Maybe Page
parse location =
    UrlParser.parseHash route location
