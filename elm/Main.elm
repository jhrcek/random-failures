module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, em, h2, h3, hr, input, strong, table, td, text, textarea, tr)
import Html.Attributes as Attr exposing (cols, href, maxlength, rows, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Input
import Json.Decode as Decode
import List.Extra
import Table
import Task
import Time
import Time.DateTime as DateTime exposing (DateTime, zero)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform SetNow Time.now )


initialModel : Model
initialModel =
    let
        failures =
            parseFailuresJson Input.failureDataJsonString

        sortedFailureTimestamps =
            List.sort <| List.map (DateTime.toTimestamp << .date) failures

        oldestDate =
            DateTime.fromTimestamp <| Maybe.withDefault 0 <| List.minimum sortedFailureTimestamps

        newestDate =
            DateTime.fromTimestamp <| Maybe.withDefault 0 <| List.maximum sortedFailureTimestamps
    in
    { groupedFailures = groupFailuresByClassAndMethod failures
    , failureCountFilter = 5
    , dateRangeFilter = ( oldestDate, newestDate )
    , tableState = Table.initialSort stdDevColumnName
    , showingDetails = Nothing
    , now = DateTime.epoch
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg model =
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

        SetNow timestamp ->
            { model | now = DateTime.fromTimestamp timestamp }


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State
    | ShowDetails ClassAndMethod
    | HideDetails
    | SetNow Time.Time


type alias Model =
    { groupedFailures : GroupedFailures
    , failureCountFilter : Int
    , dateRangeFilter : ( DateTime, DateTime )
    , tableState : Table.State
    , showingDetails : Maybe ClassAndMethod
    , now : DateTime
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
    case model.showingDetails of
        Nothing ->
            mainPage model

        Just classAndMethod ->
            failureDetailView classAndMethod model.groupedFailures


mainPage : Model -> Html Msg
mainPage model =
    div []
        [ description model.dateRangeFilter
        , filterControls model
        , failureSummaryTable model
        ]


description : ( DateTime, DateTime ) -> Html Msg
description ( fromDate, toDate ) =
    div []
        [ h2 [] [ text "Random test failure analysis" ]
        , text "This report lists all test failures in "
        , a [ href "https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/" ] [ text "kie-jenkins PR jobs" ]
        , text <| " (master branch only) from " ++ formatDate fromDate ++ " to " ++ formatDate toDate ++ "."
        ]


filterControls : Model -> Html Msg
filterControls { failureCountFilter } =
    div []
        [ text "Show tests that failed "
        , input [ type_ "number", maxlength 2, Attr.min "0", Attr.max "100", onInput ChangeFailureCountFilter, value (toString failureCountFilter) ] []
        , text " or more times"
        ]


failureSummaryTable : Model -> Html Msg
failureSummaryTable model =
    let
        acceptedFailures =
            Dict.toList model.groupedFailures
                |> List.filter (\( _, failures ) -> List.length failures >= model.failureCountFilter)
    in
    div []
        [ h3 [] [ text "Failures (grouped by Class and Test method)" ]
        , Table.view (tableConfig model.now) model.tableState acceptedFailures
        , hr [] []
        , em [] [ text "* Standard deviation of failure dates (in days)" ]
        ]


tableConfig : DateTime -> Table.Config TableRecord Msg
tableConfig now =
    Table.config
        { toId = \( ( cl, _ ), _ ) -> cl
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Class" (\( ( cl, _ ), _ ) -> fqnToSimpleClassName cl)
            , Table.stringColumn "Method" (\( ( _, m ), _ ) -> m)
            , Table.intColumn "Failures" (\( ( _, _ ), fs ) -> List.length fs)
            , stdDevColumn
            , Table.intColumn "Days since last failure" (\( _, fs ) -> daysSinceLastFailure fs now)
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
            , children = [ button [ onClick (ShowDetails ( cl, m )) ] [ text "Details >>" ] ]
            }
    in
    Table.veryCustomColumn
        { name = "Details"
        , sorter = Table.unsortable
        , viewData = detailsButton
        }


failureDetailView : ClassAndMethod -> GroupedFailures -> Html Msg
failureDetailView ( cl, m ) groupedFailures =
    let
        sortedFailures =
            getSortedFailuresOf ( cl, m ) groupedFailures

        uniqueStacktraces =
            List.Extra.unique <| List.map .stackTrace sortedFailures
    in
    div []
        [ button [ onClick HideDetails ] [ text "<< Back to Summary" ]
        , h2 [] [ text " Failure details" ]
        , table []
            [ tr []
                [ td [] [ strong [] [ text "Class: " ] ]
                , td [] [ text <| fqnToSimpleClassName cl ]
                ]
            , tr []
                [ td [] [ strong [] [ text "Method: " ] ]
                , td [] [ text m ]
                ]
            , tr []
                [ td [] [ strong [] [ text "Total failures: " ] ]
                , td [] [ text <| toString <| List.length sortedFailures ]
                ]
            , tr []
                [ td [] [ strong [] [ text "Unique stack traces: " ] ]
                , td [] [ text <| toString <| List.length uniqueStacktraces ]
                ]
            ]
        , h3 [] [ text "Failures" ]
        , div [] <| List.map viewFailure sortedFailures
        , em [] [ text "Note that some of the job links might be dead, because archived jobs are deleted after some time" ]
        , h3 [] [ text "Stack traces" ]
        , div [] <| List.map (\st -> div [] [ textarea [ value st, cols 160, rows 10 ] [] ]) uniqueStacktraces
        ]


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    Maybe.withDefault [] << Dict.get classAndMethod


viewFailure : TestFailure -> Html Msg
viewFailure { url, date, stackTrace } =
    div []
        [ text <| "Failed on " ++ formatDateTime date ++ " in job "
        , a [ href url ] [ text <| String.dropLeft 65 <| String.dropRight 11 url ]
        ]


{-| YYYY-MM-DD HH:mm
-}
formatDateTime : DateTime -> String
formatDateTime =
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


{-| YYYY-MM-DD
-}
formatDate : DateTime -> String
formatDate =
    let
        pad =
            String.padLeft 2 '0' << toString
    in
    DateTime.toTuple
        >> (\( year, month, day, _, _, _, _ ) ->
                toString year
                    ++ "-"
                    ++ pad month
                    ++ "-"
                    ++ pad day
           )


daysSinceLastFailure : List TestFailure -> DateTime -> Int
daysSinceLastFailure failures now =
    -- assuming failures are sorted by failure date
    List.reverse failures
        |> List.head
        |> Maybe.map (.date >> DateTime.delta now >> .days)
        |> Maybe.withDefault 0


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
        -- sort by failure date from oldest to most recent
        >> Dict.map (\_ failures -> List.sortBy (DateTime.toTimestamp << .date) failures)


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
