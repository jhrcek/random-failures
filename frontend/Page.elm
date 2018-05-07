module Page
    exposing
        ( ClassAndMethod
        , Page(ClassDetails, Home, MethodDetails)
        , parse
        , toClassFilter
        , toUrlHash
        )

import Http
import Navigation
import UrlParser exposing ((</>), Parser, custom, map, oneOf, s, top)


type Page
    = Home
    | ClassDetails String
    | MethodDetails ClassAndMethod (Maybe String)


type alias ClassAndMethod =
    ( String, String )


toUrlHash : Page -> String
toUrlHash page =
    let
        pieces =
            case page of
                Home ->
                    []

                ClassDetails clz ->
                    [ "class", Http.encodeUri clz ]

                MethodDetails ( clz, method ) _ ->
                    [ "class", Http.encodeUri clz, "method", Http.encodeUri method ]
    in
    "#/" ++ String.join "/" pieces


route : Parser (Page -> a) a
route =
    oneOf
        [ map Home top
        , map ClassDetails (s "class" </> uriEncodedString)
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


toClassFilter : Page -> Maybe String
toClassFilter page =
    case page of
        ClassDetails clz ->
            Just clz

        Home ->
            Nothing

        MethodDetails _ _ ->
            Nothing
