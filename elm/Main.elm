module Main exposing (main)

import Color exposing (Color)
import Color.Convert
import Dict exposing (Dict)
import Dict.Extra
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, h2, h3, hr, img, input, li, strong, table, td, text, textarea, th, tr, ul)
import Html.Attributes as Attr exposing (checked, cols, href, maxlength, name, rows, src, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Input
import Json.Decode as Decode
import List.Extra
import Navigation
import Page exposing (ClassAndMethod, Page)
import Table exposing (defaultCustomizations)
import Task
import Time
import Time.DateTime as DateTime exposing (DateTime, zero)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( initialPage, perhapsModifyUrl ) =
            maybeNavigate location
    in
    ( initialModel initialPage
    , Cmd.batch
        [ Task.perform SetNow Time.now
        , perhapsModifyUrl
        ]
    )


{-| Ensure that whenever URL is invalid, the App goes back to summary
-}
maybeNavigate : Navigation.Location -> ( Page, Cmd Msg )
maybeNavigate location =
    case Page.parse location of
        Just p ->
            ( p, Cmd.none )

        Nothing ->
            ( Page.Summary
            , Navigation.modifyUrl <| Page.toUrlHash Page.Summary
            )


initialModel : Page -> Model
initialModel initialPage =
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
    , page = initialPage
    , now = DateTime.epoch
    , fqnEnabled = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFailureCountFilter str ->
            let
                newFailureCountFilter =
                    String.toInt str |> Result.withDefault 1
            in
            { model | failureCountFilter = newFailureCountFilter } ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        SetNow timestamp ->
            { model | now = DateTime.fromTimestamp timestamp } ! []

        ToggleFQN flag ->
            { model | fqnEnabled = flag } ! []

        ToggleStacktrace st ->
            case model.page of
                Page.MethodDetails classAndMethod _ ->
                    { model | page = Page.MethodDetails classAndMethod (Just st) } ! []

                _ ->
                    model ! []

        UrlChange location ->
            let
                ( newPage, perhapsModifyUrl ) =
                    maybeNavigate location
            in
            { model | page = newPage } ! [ perhapsModifyUrl ]


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State
    | SetNow Time.Time
    | ToggleFQN Bool
    | ToggleStacktrace String
    | UrlChange Navigation.Location


type alias Model =
    { groupedFailures : GroupedFailures
    , failureCountFilter : Int
    , dateRangeFilter : ( DateTime, DateTime )
    , tableState : Table.State
    , page : Page
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


view : Model -> Html Msg
view model =
    case model.page of
        Page.Summary ->
            summaryView model

        Page.MethodDetails classAndMethod mSt ->
            let
                failures =
                    getSortedFailuresOf classAndMethod model.groupedFailures
            in
            if List.isEmpty failures then
                classAndMethodNotFoundView classAndMethod
            else
                failureDetailView classAndMethod failures model.dateRangeFilter mSt


summaryView : Model -> Html Msg
summaryView model =
    div []
        [ description model.dateRangeFilter
        , filterControls model
        , failureSummaryTable model
        ]


classAndMethodNotFoundView : ClassAndMethod -> Html Msg
classAndMethodNotFoundView ( clz, method ) =
    div []
        [ backToSummaryLink
        , h2 [] [ text <| "No failures found" ]
        , table
            []
            [ tr []
                [ td [] [ strong [] [ text "Class" ] ]
                , td [] [ text clz ]
                ]
            , tr []
                [ td [] [ strong [] [ text "Method" ] ]
                , td [] [ text method ]
                ]
            ]
        , div [] [ text "Are you sure you've got the class and method name right?" ]
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
filterControls { failureCountFilter, fqnEnabled } =
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
            [ text "Class Names"
            , input [ type_ "radio", name "fqn", checked (not fqnEnabled), onClick (ToggleFQN False) ] []
            , text "Simple"
            , input [ type_ "radio", name "fqn", checked fqnEnabled, onClick (ToggleFQN True) ] []
            , text "Fully Qualified"
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
            [ text <|
                "The following heuristic is used to highlight random failures. "
                    ++ "A test is considered randomly failing if all of the following conditions hold (open to discussion!):"
            , ul []
                [ li [] [ text "Failed 5 or more times" ]
                , li [] [ text "Last failure ocurred no longer than 14 days ago" ]
                , li [] [ text "Standard deviation of failure dates is greater than 5 days" ]
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
    failureCount >= 5 && lastFailureDaysAgo < 14 && failureDatesStdDev > 5


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
        detailsLink ( classAndMethod, _ ) =
            { attributes = []
            , children = [ a [ href (Page.toUrlHash (Page.MethodDetails classAndMethod Nothing)) ] [ text "details >>" ] ]
            }
    in
    Table.veryCustomColumn
        { name = "Details"
        , sorter = Table.unsortable
        , viewData = detailsLink
        }


failureDetailView : ClassAndMethod -> List TestFailure -> ( DateTime, DateTime ) -> Maybe String -> Html Msg
failureDetailView ( cl, m ) sortedFailures dateRange mStackTrace =
    let
        stacktraces =
            List.map .stackTrace sortedFailures

        uniqueStacktracesAndMessages =
            List.Extra.unique stacktraces

        uniqueStacktraces =
            List.Extra.uniqueBy (\st -> String.split "\t" st |> List.tail |> toString) stacktraces

        maybeStacktraceView =
            Maybe.withDefault (text "") <| Maybe.map stacktraceView mStackTrace

        colorizedFailures =
            assignColorsToStacktraces sortedFailures
    in
    div []
        [ backToSummaryLink
        , h2 [] [ text "Failure details" ]
        , failureDetailsSummary cl m (List.length sortedFailures) (List.length uniqueStacktracesAndMessages) (List.length uniqueStacktraces)
        , h3 [] [ text "Spread of failure dates" ]
        , viewFailureDatesChart dateRange colorizedFailures
        , h3 [] [ text "Failures" ]
        , failuresTable colorizedFailures
        , maybeStacktraceView
        ]


backToSummaryLink : Html Msg
backToSummaryLink =
    a [ href (Page.toUrlHash Page.Summary) ] [ text "<< back to summary" ]


assignColorsToStacktraces : List TestFailure -> List ( TestFailure, Color )
assignColorsToStacktraces failures =
    let
        stacktraceToColor : Dict String Color
        stacktraceToColor =
            List.map .stackTrace failures
                |> List.Extra.unique
                |> (\uniqueStacktraces -> List.map2 (\st color -> ( st, color )) uniqueStacktraces stacktraceColors)
                |> Dict.fromList

        assignColor : TestFailure -> Color
        assignColor f =
            Dict.get f.stackTrace stacktraceToColor |> Maybe.withDefault Color.white
    in
    List.map (\f -> ( f, assignColor f )) failures


stacktraceColors : List Color
stacktraceColors =
    [ Color.lightRed
    , Color.lightOrange
    , Color.lightYellow
    , Color.lightGreen
    , Color.lightBlue
    , Color.lightPurple
    , Color.lightBrown
    , Color.darkRed
    , Color.darkOrange
    , Color.darkYellow
    , Color.darkGreen
    , Color.darkBlue
    , Color.darkPurple
    , Color.darkBrown
    , Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    , Color.grey
    , Color.darkGrey
    , Color.lightCharcoal
    , Color.charcoal
    , Color.darkCharcoal
    ]


failureDetailsSummary : String -> String -> Int -> Int -> Int -> Html Msg
failureDetailsSummary className methodName totalFailures uniqueStacktracesAndMessagesCount uniqueStacktracesCount =
    table []
        [ tr []
            [ td [] [ strong [] [ text "Class" ] ]
            , td [] [ text className ]
            ]
        , tr []
            [ td [] [ strong [] [ text "Method" ] ]
            , td [] [ text methodName ]
            ]
        , tr []
            [ td [] [ strong [] [ text "Total failures" ] ]
            , td [] [ text <| toString totalFailures ]
            ]
        , tr []
            [ td []
                [ strong [] [ text "Unique stack traces (including ex. message)" ]
                , helpIcon <|
                    "Total number unique stack traces including exception message "
                        ++ "(looking at both WHERE the failure occured AND the exception message)"
                ]
            , td [] [ text <| toString uniqueStacktracesAndMessagesCount ]
            ]
        , tr []
            [ td []
                [ strong [] [ text "Unique stack traces" ]
                , helpIcon <|
                    "Total number unique stack traces that are different disregarding exception message "
                        ++ "(just looking at WHERE the failure was, ignoring exception message)"
                ]
            , td [] [ text <| toString uniqueStacktracesCount ]
            ]
        ]


stacktraceView : String -> Html Msg
stacktraceView stackTrace =
    div []
        [ h3 [] [ text "Stack Trace" ]
        , textarea
            [ value stackTrace
            , cols 160
            , rows <| 1 + List.length (String.lines stackTrace)
            ]
            []
        ]


helpIcon : String -> Html a
helpIcon helpText =
    img
        [ src "images/q.png"
        , title helpText
        , style
            [ ( "width", "15px" )
            , ( "height", "15px" )
            , ( "margin-left", "4px" )
            , ( "margin-right", "4px" )
            , ( "margin-bottom", "-2px" )
            ]
        ]
        []


viewFailureDatesChart : ( DateTime, DateTime ) -> List ( TestFailure, Color ) -> Html a
viewFailureDatesChart ( fromDate, toDate ) colorizedFailures =
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
        (\( failure, color ) ->
            div
                [ style
                    [ ( "width", "10px" )
                    , ( "height", "10px" )
                    , ( "border-radius", "5px" )
                    , ( "position", "absolute" )
                    , ( "background-color", Color.Convert.colorToHex color )
                    , ( "left", toString (relativePosition (DateTime.toTimestamp failure.date)) ++ "%" )
                    , ( "transform", "rotate(-45deg)" )
                    , ( "white-space", "pre" )
                    ]
                ]
                [ text <| "   " ++ formatDateTime failure.date ]
        )
        colorizedFailures
        |> div
            [ style
                [ ( "background-color", "lightgray" )
                , ( "position", "relative" )
                , ( "height", "10px" )
                , ( "margin-top", "80px" )
                , ( "width", "95%" )
                ]
            ]


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    Maybe.withDefault [] << Dict.get classAndMethod


failuresTable : List ( TestFailure, Color ) -> Html Msg
failuresTable colorizedFailures =
    table [] <|
        [ tr []
            [ th [] [ text "Failed on" ]
            , th [] [ text "Build URL", helpIcon "Some of the job links might be dead, because archived jobs are deleted after some time" ]
            , th [] [ text "Unique Stack Trace" ]
            , th [] [ text "Action" ]
            ]
        ]
            ++ List.map failureRow colorizedFailures


failureRow : ( TestFailure, Color ) -> Html Msg
failureRow ( { url, date, stackTrace }, color ) =
    tr []
        [ td [] [ text <| formatDateTime date ]
        , td [] [ a [ href url ] [ text <| String.dropLeft 65 <| String.dropRight 11 url ] ]
        , td [ style [ ( "background-color", Color.Convert.colorToHex color ) ] ] []
        , td [] [ button [ onClick (ToggleStacktrace stackTrace) ] [ text "Show Stack Trace" ] ]
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
