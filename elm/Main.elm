module Main exposing (main)

import Dict exposing (Dict)
import Dict.Extra
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, h2, h3, hr, input, label, li, strong, table, td, text, textarea, tr, ul)
import Html.Attributes as Attr exposing (cols, href, maxlength, rows, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Input
import Json.Decode as Decode
import List.Extra
import Table exposing (defaultCustomizations)
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
    , failureCountFilter = 3
    , dateRangeFilter = ( oldestDate, newestDate )
    , tableState = Table.initialSort stdDevColumnName
    , showingDetails = Nothing
    , now = DateTime.epoch
    , fqnEnabled = False
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

        ToggleFQN flag ->
            { model | fqnEnabled = flag }


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State
    | ShowDetails ClassAndMethod
    | HideDetails
    | SetNow Time.Time
    | ToggleFQN Bool


type alias Model =
    { groupedFailures : GroupedFailures
    , failureCountFilter : Int
    , dateRangeFilter : ( DateTime, DateTime )
    , tableState : Table.State
    , showingDetails : Maybe ClassAndMethod
    , now : DateTime
    , fqnEnabled : Bool
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
            failureDetailView classAndMethod model.groupedFailures model.dateRangeFilter


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
        , text "This report was generated on GENERATED_ON_PLACEHOLDER and lists all test failures in "
        , a [ href "https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/" ] [ text "kie-jenkins PR jobs" ]
        , text <| " (master only) from " ++ formatDate fromDate ++ " to " ++ formatDate toDate ++ "."
        ]


filterControls : Model -> Html Msg
filterControls { failureCountFilter } =
    div []
        [ text "Show tests that failed "
        , input
            [ type_ "number"
            , maxlength 2
            , Attr.min "0"
            , Attr.max "100"
            , onInput ChangeFailureCountFilter
            , value (toString failureCountFilter)
            , style [ ( "width", "50px" ) ]
            ]
            []
        , text " or more times"
        , div []
            [ label []
                [ input [ type_ "checkbox", onCheck ToggleFQN ] []
                , text "Show Fully Qualified Class Names"
                ]
            ]
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
        , Table.view (tableConfig model.now model.fqnEnabled) model.tableState acceptedFailures
        , summaryLegend
        ]


summaryLegend : Html Msg
summaryLegend =
    div []
        [ hr [] []
        , div [] [ text "* Standard deviation of failure dates (in days)" ]
        , h2 [] [ text "When is test method considered to be failing randomly?" ]
        , div []
            [ text "The following heuristic is used to highlight random failures. A test is considered randomly failing if all of the following conditions hold (open to discussion!):"
            , ul []
                [ li [] [ text "Failed 3 or more times" ]
                , li [] [ text "Last failure ocurred no longer than 14 days ago" ]
                , li [] [ text "Standard deviation of failure dates is 4 days or more" ]
                ]
            ]
        , h2 [] [ text "Got ideas about how to make this report more useful?" ]
        , text "File an issue on "
        , a [ href "https://github.com/jhrcek/random-failures/issues" ] [ text "project's page" ]
        ]


tableConfig : DateTime -> Bool -> Table.Config TableRecord Msg
tableConfig now fqnEnabled =
    Table.customConfig
        { toId = \( ( cl, _ ), _ ) -> cl
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Class"
                (\( ( cl, _ ), _ ) ->
                    if fqnEnabled then
                        cl
                    else
                        fqnToSimpleClassName cl
                )
            , Table.stringColumn "Method" (\( ( _, m ), _ ) -> m)
            , Table.intColumn "Failures" (\( ( _, _ ), fs ) -> List.length fs)
            , stdDevColumn
            , Table.intColumn "Days since last failure" (\( _, fs ) -> daysSinceLastFailure fs now)
            , detailsColumn
            ]
        , customizations =
            { defaultCustomizations
                | rowAttrs =
                    \( _, fs ) ->
                        if isProbablyRandom fs now then
                            [ style [ ( "background-color", "salmon" ) ] ]
                        else
                            []
            }
        }


isProbablyRandom : List TestFailure -> DateTime -> Bool
isProbablyRandom fs now =
    let
        lastFailureDaysAgo =
            daysSinceLastFailure fs now

        failureDatesStdDev =
            standardDeviation <| List.map .date fs

        failureCount =
            List.length fs
    in
    failureCount >= 3 && lastFailureDaysAgo < 14 && failureDatesStdDev > 5


stdDevColumn : Table.Column TableRecord Msg
stdDevColumn =
    let
        getDeviation ( _, fs ) =
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


failureDetailView : ClassAndMethod -> GroupedFailures -> ( DateTime, DateTime ) -> Html Msg
failureDetailView ( cl, m ) groupedFailures dateRange =
    let
        sortedFailures =
            getSortedFailuresOf ( cl, m ) groupedFailures

        failureTimestamps =
            List.map (DateTime.toTimestamp << .date) sortedFailures

        stacktraces =
            List.map .stackTrace sortedFailures

        uniqueStacktracesAndMessages =
            List.Extra.unique stacktraces

        uniqueStacktraces =
            List.Extra.uniqueBy (\st -> String.split "\t" st |> List.tail |> toString) stacktraces

        getRowsToDisplay stacktrace =
            min 10 (1 + List.length (String.lines stacktrace))
    in
    div []
        [ button [ onClick HideDetails ] [ text "<< Back to Summary" ]
        , h2 [] [ text " Failure details" ]
        , table []
            [ tr []
                [ td [] [ strong [] [ text "Class: " ] ]
                , td [] [ text cl ]
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
                [ td [] [ strong [] [ text "Unique stack traces (including ex. message): ", a [ href "#one" ] [ text "(1)" ] ] ]
                , td [] [ text <| toString <| List.length uniqueStacktracesAndMessages ]
                ]
            , tr []
                [ td [] [ strong [] [ text "Unique stack traces: ", a [ href "#two" ] [ text "(2)" ] ] ]
                , td [] [ text <| toString <| List.length uniqueStacktraces ]
                ]
            ]
        , h3 [] [ text "Spread of failure dates" ]
        , viewFailureDatesChart dateRange failureTimestamps
        , h3 [] [ text "Failures ", a [ href "#three" ] [ text "(3)" ] ]
        , div [] <| List.map viewFailure sortedFailures
        , h3 [] [ text "Unique Stack Traces" ]
        , div [] <| List.map (\st -> div [] [ textarea [ value st, cols 160, rows <| getRowsToDisplay st ] [] ]) uniqueStacktracesAndMessages
        , detailsLegend
        ]


detailsLegend : Html Msg
detailsLegend =
    div []
        [ hr [] []
        , div [ Attr.id "one" ] [ text "(1) Total number unique stack traces including exception message (looking at both WHERE the failure occured AND the exception message)" ]
        , div [ Attr.id "two" ] [ text "(2) Total number unique stack traces that are different disregarding exception message (just looking at WHERE the failure was, ignoring exception message)" ]
        , div [ Attr.id "three" ] [ text "(3) Some of the job links might be dead, because archived jobs are deleted after some time" ]
        ]


viewFailureDatesChart : ( DateTime, DateTime ) -> List Float -> Html a
viewFailureDatesChart ( fromDate, toDate ) timestamps =
    let
        leastTimestamp =
            DateTime.toTimestamp fromDate

        biggestTimestamp =
            DateTime.toTimestamp toDate

        timestampRange =
            biggestTimestamp - leastTimestamp

        relativePosition : Float -> Float
        relativePosition t =
            100 * (t - leastTimestamp) / timestampRange
    in
    List.map
        (\t ->
            div
                [ style
                    [ ( "width", "10px" )
                    , ( "height", "10px" )
                    , ( "border-radius", "5px" )
                    , ( "position", "absolute" )
                    , ( "background-color", "red" )
                    , ( "left", toString (relativePosition t) ++ "%" )
                    , ( "transform", "rotate(-45deg)" )
                    , ( "white-space", "pre" )
                    ]
                ]
                [ text <| "   " ++ formatDateTime (DateTime.fromTimestamp t) ]
        )
        timestamps
        |> div
            [ style
                [ ( "background-color", "lightgray" )
                , ( "position", "relative" )
                , ( "height", "10px" )
                , ( "margin-top", "80px" )
                ]
            ]


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    Maybe.withDefault [] << Dict.get classAndMethod


viewFailure : TestFailure -> Html Msg
viewFailure { url, date } =
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
