module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, em, h3, input, strong, text)
import Html.Attributes exposing (href, maxlength, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Input
import Json.Decode as Decode
import Table
import Time
import Time.DateTime as DateTime exposing (DateTime, zero)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


initialModel : Model
initialModel =
    { groupedFailures = groupFailuresByClassAndMethod <| parseFailuresJson Input.failureDataJsonString
    , failureCountFilter = 3
    , tableState = Table.initialSort stdDevColumnName
    , showingDetails = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeFailureCountFilter str ->
            let
                newFailureCountFilter =
                    String.toInt str |> Result.withDefault 1
            in
            { model | failureCountFilter = newFailureCountFilter }

        SetTableState newState ->
            { model | tableState = newState }

        ShowDetails classAndMethod ->
            { model | showingDetails = Just classAndMethod }

        HideDetails ->
            { model | showingDetails = Nothing }


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State
    | ShowDetails ClassAndMethod
    | HideDetails


type alias Model =
    { groupedFailures : GroupedFailures
    , failureCountFilter : Int
    , tableState : Table.State
    , showingDetails : Maybe ClassAndMethod
    }


type alias TestFailure =
    { url : String
    , date : DateTime
    , testClass : String
    , testMethod : String
    , stackTrace : String
    }


type alias GroupedFailures =
    Dict ClassAndMethod (List TestFailure)


type alias TableRecord =
    ( ClassAndMethod, List TestFailure )


type alias ClassAndMethod =
    ( String, String )


view : Model -> Html Msg
view model =
    div []
        [ filterControls model.failureCountFilter
        , failuresTable model
        , detailsView model
        ]


filterControls : Int -> Html Msg
filterControls failureCountFilter =
    div []
        [ h3 [] [ text "Filter failures" ]
        , text "Show tests that failed "
        , input [ type_ "number", maxlength 2, size 2, onInput ChangeFailureCountFilter, value (toString failureCountFilter) ] []
        , text " or more times"
        ]


failuresTable : Model -> Html Msg
failuresTable model =
    let
        acceptedFailures =
            Dict.toList model.groupedFailures
                |> List.filter (\( _, failures ) -> List.length failures >= model.failureCountFilter)
    in
    div []
        [ h3 [] [ text "Failures aggregated by class and test method" ]
        , Table.view tableConfig model.tableState acceptedFailures
        , em [] [ text "* Standard deviation of failure dates (in days)" ]
        ]


tableConfig : Table.Config TableRecord Msg
tableConfig =
    Table.config
        { toId = \( ( cl, m ), fs ) -> cl
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Class" (\( ( cl, m ), fs ) -> fqnToSimpleClassName cl)
            , Table.stringColumn "Method" (\( ( cl, m ), fs ) -> m)
            , Table.intColumn "Failures" (\( ( cl, m ), fs ) -> List.length fs)
            , stdDevColumn
            , detailsColumn
            ]
        }


stdDevColumn : Table.Column TableRecord Msg
stdDevColumn =
    let
        getDeviation ( ( cl, m ), fs ) =
            standardDeviation <| List.map .date fs
    in
    Table.customColumn
        { name = stdDevColumnName
        , viewData = FormatNumber.format usLocale << getDeviation
        , sorter = Table.decreasingOrIncreasingBy getDeviation
        }


stdDevColumnName : String
stdDevColumnName =
    "Spread of failure dates *"


detailsColumn : Table.Column TableRecord Msg
detailsColumn =
    let
        detailsButton ( ( cl, m ), _ ) =
            { attributes = []
            , children = [ button [ onClick (ShowDetails ( cl, m )) ] [ text "Details" ] ]
            }
    in
    Table.veryCustomColumn
        { name = "Details"
        , sorter = Table.unsortable
        , viewData = detailsButton
        }


detailsView : Model -> Html Msg
detailsView model =
    case model.showingDetails of
        Nothing ->
            text ""

        Just ( cl, m ) ->
            div []
                [ h3 [] [ text "Failure details ", button [ onClick HideDetails ] [ text "hide" ] ]
                , div []
                    [ strong [] [ text "Class: " ]
                    , text <| fqnToSimpleClassName cl
                    , strong [] [ text " Method: " ]
                    , text m
                    ]
                , div [] <| List.map viewFailure <| getSortedFailuresOf ( cl, m ) model.groupedFailures
                , em [] [ text "Note that some of the job links might be dead, because archived jobs are deleted after some time" ]
                ]


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    List.sortBy (DateTime.toTimestamp << .date) << Maybe.withDefault [] << Dict.get classAndMethod


viewFailure : TestFailure -> Html Msg
viewFailure { url, date, stackTrace } =
    div []
        [ text <| "Failed on " ++ formatDate date ++ " in job "
        , a [ href url ] [ text <| String.dropLeft 65 <| String.dropRight 11 url ]
        ]


formatDate : DateTime -> String
formatDate =
    let
        pad =
            String.padLeft 2 '0' << toString
    in
    DateTime.toTuple
        >> (\( year, month, day, hour, minute, _, _ ) ->
                toString year
                    ++ "-"
                    ++ pad month
                    ++ "-"
                    ++ pad day
                    ++ " "
                    ++ pad hour
                    ++ ":"
                    ++ pad minute
           )


fqnToSimpleClassName : String -> String
fqnToSimpleClassName fqn =
    String.split "." fqn |> List.reverse |> List.head |> Maybe.withDefault fqn


parseFailuresJson : String -> List TestFailure
parseFailuresJson =
    Result.withDefault [] << Decode.decodeString (Decode.list testFailureDecoder)


standardDeviation : List DateTime -> Float
standardDeviation dts =
    let
        ts =
            List.map DateTime.toTimestamp dts

        len =
            toFloat <| List.length ts

        avg =
            List.sum ts / len

        summedSquares =
            List.sum <| List.map (\x -> (x - avg) ^ 2) ts
    in
    (\hours -> hours / 24) <| Time.inHours <| sqrt <| summedSquares / len


groupFailuresByClassAndMethod : List TestFailure -> GroupedFailures
groupFailuresByClassAndMethod =
    Dict.Extra.groupBy (\failure -> ( failure.testClass, failure.testMethod ))


testFailureDecoder : Decode.Decoder TestFailure
testFailureDecoder =
    Decode.map5 TestFailure
        (Decode.field "url" Decode.string)
        (Decode.field "date" dateTimeDecoder)
        (Decode.field "testClass" Decode.string)
        (Decode.field "testMethod" Decode.string)
        (Decode.field "stackTrace" Decode.string)


dateTimeDecoder : Decode.Decoder DateTime
dateTimeDecoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\xs ->
                case xs of
                    [ yr, mn, d, h, m ] ->
                        Decode.succeed <| DateTime.dateTime { zero | year = yr, month = mn, day = d, hour = h, minute = m }

                    flds ->
                        Decode.fail <| "Unable to parse DateTime from fields" ++ toString flds
            )
