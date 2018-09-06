module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Color exposing (Color)
import Dict exposing (Dict)
import Dict.Extra
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, h2, h3, img, input, li, pre, span, strong, table, td, text, th, tr, ul)
import Html.Attributes as Attr exposing (checked, class, cols, href, maxlength, name, rows, src, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as Decode
import List.Extra
import Page exposing (ClassAndMethod, Page)
import RemoteData exposing (RemoteData(..), WebData)
import Table exposing (defaultCustomizations)
import Task
import Time exposing (Month(..), Posix)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        initialPage =
            Page.fromUrl url
    in
    ( initialModel initialPage key
    , Cmd.batch
        [ Task.perform SetNow Time.now
        , loadFailures
        ]
    )


navigateTo : Url -> Model -> Model
navigateTo url model =
    let
        newPage =
            Page.fromUrl url

        newClassFilter =
            Page.toClassFilter newPage
    in
    { model
        | page = newPage
        , filters = setClassFilter newClassFilter model.filters
    }


loadFailures : Cmd Msg
loadFailures =
    Http.get "failures.json" failureListDecoder
        |> RemoteData.sendRequest
        |> Cmd.map FailuresLoaded


initialModel : Page -> Key -> Model
initialModel initialPage key =
    { groupedFailures = RemoteData.Loading
    , filters =
        { failureCount = 5
        , dateRange = ( epoch, epoch )
        , className = Page.toClassFilter initialPage
        }
    , tableState = Table.initialSort daysSinceLastFailureColumnName
    , page = initialPage
    , now = epoch
    , fqnEnabled = False
    , navKey = key
    }


type alias Filters =
    { failureCount : Int
    , dateRange : ( Posix, Posix )
    , className : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFailureCountFilter failureCountStr ->
            ( { model | filters = setFailureCountFilter failureCountStr model.filters }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        SetNow posix ->
            ( { model | now = posix }
            , Cmd.none
            )

        ToggleFQN flag ->
            ( { model | fqnEnabled = flag }
            , Cmd.none
            )

        ShowStackTrace st ->
            ( setStackTrace (Just st) model
            , Cmd.none
            )

        HideStackTrace ->
            ( setStackTrace Nothing model
            , Cmd.none
            )

        UrlChange url ->
            ( navigateTo url model
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                      -- Push new url to browser history, model is loaded in UrlChange branch
                    , Page.fromUrl url
                        |> Page.toUrlHash
                        |> Nav.pushUrl model.navKey
                    )

                External url ->
                    ( model, Nav.load url )

        FailuresLoaded failuresRemoteData ->
            let
                dateRange =
                    RemoteData.map calculateDateRange failuresRemoteData
                        |> RemoteData.withDefault ( epoch, epoch )
            in
            ( { model
                | groupedFailures = RemoteData.map groupFailuresByClassAndMethod failuresRemoteData
                , filters = setDateRangeFilter dateRange model.filters
              }
            , Cmd.none
            )


setStackTrace : Maybe String -> Model -> Model
setStackTrace maybeStackTrace model =
    case model.page of
        Page.MethodDetails classAndMethod _ ->
            { model | page = Page.MethodDetails classAndMethod maybeStackTrace }

        _ ->
            model


setFailureCountFilter : String -> Filters -> Filters
setFailureCountFilter fcStr filters =
    let
        newFailureCount =
            String.toInt fcStr |> Maybe.withDefault 1
    in
    { filters | failureCount = newFailureCount }


setDateRangeFilter : ( Posix, Posix ) -> Filters -> Filters
setDateRangeFilter rng filters =
    { filters | dateRange = rng }


setClassFilter : Maybe String -> Filters -> Filters
setClassFilter maybeClassName filters =
    { filters | className = maybeClassName }


calculateDateRange : List TestFailure -> ( Posix, Posix )
calculateDateRange failures =
    let
        failureDates =
            List.map .date failures

        oldestDate =
            Maybe.withDefault epoch <| List.Extra.minimumBy Time.posixToMillis failureDates

        newestDate =
            Maybe.withDefault epoch <| List.Extra.maximumBy Time.posixToMillis failureDates
    in
    ( oldestDate, newestDate )


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State
    | SetNow Posix
    | ToggleFQN Bool
    | ShowStackTrace String
    | HideStackTrace
    | UrlChange Url
    | LinkClicked UrlRequest
    | FailuresLoaded (WebData (List TestFailure))


type alias Model =
    { groupedFailures : WebData GroupedFailures
    , filters : Filters
    , tableState : Table.State
    , page : Page
    , now : Posix
    , fqnEnabled : Bool
    , navKey : Key
    }


type alias TestFailure =
    { url : String
    , date : Posix
    , testClass : String
    , testMethod : String
    , stackTrace : String
    }


type alias GroupedFailures =
    Dict ClassAndMethod (List TestFailure)


type alias TableRecord =
    ( ClassAndMethod, List TestFailure )


{-| When the failure comes from setup method (@Before, @After etc), Jenkins uses class name as method name
-}
getMethodName : ClassAndMethod -> String
getMethodName ( className, methodName ) =
    if className == methodName then
        "JUnit setup method (@Before, @After etc.)"

    else
        methodName


view : Model -> Document Msg
view model =
    let
        body =
            case model.groupedFailures of
                NotAsked ->
                    -- can't happen as we're beginning in the Loading state
                    [ text "Request to load failures.json wasn't sent" ]

                Loading ->
                    [ text "Loading data ..." ]

                Failure httpError ->
                    [ text <| showHttpError httpError ]

                Success loadedFailures ->
                    case model.page of
                        Page.Home ->
                            homeView model

                        Page.ClassDetails fqcn ->
                            classDetailsView fqcn model

                        Page.MethodDetails classAndMethod mSt ->
                            let
                                failures =
                                    getSortedFailuresOf classAndMethod loadedFailures
                            in
                            if List.isEmpty failures then
                                classAndMethodNotFoundView classAndMethod

                            else
                                failureDetailView classAndMethod failures model.filters.dateRange mSt
    in
    { title = "kiegroup CI Test Failures", body = body }


homeView : Model -> List (Html Msg)
homeView model =
    [ description model.filters.dateRange
    , filterControls model
    , h3 [] [ text "Failures (grouped by Class and Test method)" ]
    , failureSummaryTable model
    , faq
    ]


classDetailsView : String -> Model -> List (Html Msg)
classDetailsView fqcn model =
    [ homeLink
    , h3 [] [ text <| "Failures in class " ++ fqcn ]
    , failureSummaryTable model
    ]


classAndMethodNotFoundView : ClassAndMethod -> List (Html Msg)
classAndMethodNotFoundView ( clz, method ) =
    [ homeLink
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


description : ( Posix, Posix ) -> Html Msg
description ( fromDate, toDate ) =
    div []
        [ h2 [] [ text "Random test failure analysis" ]
        , text "This report was generated on GENERATED_ON_PLACEHOLDER and lists most"
        , helpIcon "Builds with more than 50 test failures are excluded, because they add a lot of data without providing much value for flaky test identification."
        , text "test failures in "
        , a [ href "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/" ] [ text "rhba-jenkins PR jobs" ]
        , text <| " (master only) from " ++ formatDate fromDate ++ " to " ++ formatDate toDate ++ "."
        ]


filterControls : Model -> Html Msg
filterControls { filters, fqnEnabled } =
    div []
        [ text "Show tests that failed "
        , input
            [ type_ "number"
            , maxlength 2
            , Attr.min "0"
            , Attr.max "100"
            , onInput ChangeFailureCountFilter
            , value (String.fromInt filters.failureCount)
            , style "width" "50px"
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
    case model.groupedFailures of
        Success failureData ->
            let
                acceptedFailures =
                    applyFilters model.filters failureData
            in
            div []
                [ Table.view (tableConfig model.now model.fqnEnabled) model.tableState acceptedFailures
                , div [] [ text "* Standard deviation of failure dates (in days)" ]
                ]

        _ ->
            div [] [ text "No data available" ]


applyFilters : Filters -> GroupedFailures -> List ( ClassAndMethod, List TestFailure )
applyFilters filters groupedFailures =
    Dict.toList groupedFailures
        |> List.filter (\( _, failures ) -> filters.failureCount <= List.length failures)
        |> List.filter (\( ( fqcn, _ ), _ ) -> filters.className |> Maybe.map (\chosenFqcn -> chosenFqcn == fqcn) |> Maybe.withDefault True)


faq : Html Msg
faq =
    div []
        [ h2 [] [ text "When is test method considered to be failing randomly?" ]
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


tableConfig : Posix -> Bool -> Table.Config TableRecord Msg
tableConfig now fqnEnabled =
    Table.customConfig
        { toId = \( ( fqcn, method ), _ ) -> fqcn ++ method
        , toMsg = SetTableState
        , columns =
            [ classColumn fqnEnabled
            , methodColumn
            , Table.intColumn "Failures" (\( ( _, _ ), fs ) -> List.length fs)
            , stdDevColumn
            , Table.intColumn daysSinceLastFailureColumnName (\( _, fs ) -> daysSinceLastFailure fs now)
            ]
        , customizations =
            { defaultCustomizations
                | rowAttrs =
                    \( _, fs ) ->
                        if isProbablyRandom fs now then
                            [ style "background-color" "salmon" ]

                        else
                            []
            }
        }


daysSinceLastFailureColumnName : String
daysSinceLastFailureColumnName =
    "Days since last failure"


isProbablyRandom : List TestFailure -> Posix -> Bool
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


classColumn : Bool -> Table.Column TableRecord Msg
classColumn fqnEnabled =
    let
        getVisibleClassName ( ( fqcn, _ ), _ ) =
            if fqnEnabled then
                fqcn

            else
                fqnToSimpleClassName fqcn

        classDetailsLink (( ( fqcn, _ ), _ ) as record) =
            { attributes = []
            , children =
                [ a [ href <| Page.toUrlHash <| Page.ClassDetails fqcn ]
                    [ text <| getVisibleClassName record
                    ]
                ]
            }
    in
    Table.veryCustomColumn
        { name = "Class"
        , sorter = Table.increasingOrDecreasingBy getVisibleClassName
        , viewData = classDetailsLink
        }


methodColumn : Table.Column TableRecord Msg
methodColumn =
    let
        detailsLink ( classAndMethod, _ ) =
            { attributes = []
            , children =
                [ a [ href (Page.toUrlHash (Page.MethodDetails classAndMethod Nothing)) ]
                    [ text <| getMethodName classAndMethod ]
                ]
            }
    in
    Table.veryCustomColumn
        { name = "Method"
        , sorter = Table.increasingOrDecreasingBy (\( classAndMethod, _ ) -> getMethodName classAndMethod)
        , viewData = detailsLink
        }


stdDevColumn : Table.Column TableRecord Msg
stdDevColumn =
    let
        getDeviation ( _, fs ) =
            standardDeviation <| List.map .date fs
    in
    Table.customColumn
        { name = "Spread of failure dates *"
        , viewData = FormatNumber.format usLocale << getDeviation
        , sorter = Table.decreasingOrIncreasingBy getDeviation
        }


failureDetailView : ClassAndMethod -> List TestFailure -> ( Posix, Posix ) -> Maybe String -> List (Html Msg)
failureDetailView classAndMethod sortedFailures dateRange mStackTrace =
    let
        stacktraces =
            List.map .stackTrace sortedFailures

        uniqueStacktracesAndMessages =
            List.Extra.unique stacktraces

        uniqueStacktraces =
            List.Extra.uniqueBy (\st -> String.split "\t" st |> List.tail |> Maybe.withDefault [] |> String.concat) stacktraces

        maybeStacktraceView =
            Maybe.withDefault (text "") <| Maybe.map stacktraceView mStackTrace

        colorizedFailures =
            assignColorsToStacktraces sortedFailures
    in
    [ homeLink
    , h2 [] [ text "Failure details" ]
    , failureDetailsSummary classAndMethod (List.length sortedFailures) (List.length uniqueStacktracesAndMessages) (List.length uniqueStacktraces)
    , h3 [] [ text "Spread of failure dates" ]
    , viewFailureDatesChart dateRange colorizedFailures
    , h3 [] [ text "Failures" ]
    , failuresTable colorizedFailures
    , maybeStacktraceView
    ]


homeLink : Html Msg
homeLink =
    a [ href (Page.toUrlHash Page.Home) ] [ text "<< home" ]


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


failureDetailsSummary : ClassAndMethod -> Int -> Int -> Int -> Html Msg
failureDetailsSummary (( fqcn, _ ) as classAndMethod) totalFailures uniqueStacktracesAndMessagesCount uniqueStacktracesCount =
    table []
        [ tr []
            [ td [] [ strong [] [ text "Class" ] ]
            , td []
                [ a [ href <| Page.toUrlHash <| Page.ClassDetails fqcn ]
                    [ text fqcn ]
                ]
            ]
        , tr []
            [ td [] [ strong [] [ text "Method" ] ]
            , td [] [ text (getMethodName classAndMethod) ]
            ]
        , tr []
            [ td [] [ strong [] [ text "Total failures" ] ]
            , td [] [ text <| String.fromInt totalFailures ]
            ]
        , tr []
            [ td []
                [ strong [] [ text "Unique stack traces (including ex. message)" ]
                , helpIcon <|
                    "Total number unique stack traces including exception message "
                        ++ "(looking at both WHERE the failure occured AND the exception message)"
                ]
            , td [] [ text <| String.fromInt uniqueStacktracesAndMessagesCount ]
            ]
        , tr []
            [ td []
                [ strong [] [ text "Unique stack traces" ]
                , helpIcon <|
                    "Total number unique stack traces that are different disregarding exception message "
                        ++ "(just looking at WHERE the failure was, ignoring exception message)"
                ]
            , td [] [ text <| String.fromInt uniqueStacktracesCount ]
            ]
        ]


stacktraceView : String -> Html Msg
stacktraceView stackTrace =
    div [ class "stacktrace" ]
        [ h3 [ style "margin-top" "0px" ]
            [ text "Stack Trace"
            , span [ class "stacktrace-closer", onClick HideStackTrace ] [ text "×" ]
            ]
        , pre [] [ text stackTrace ]
        ]


helpIcon : String -> Html a
helpIcon helpText =
    img
        [ class "hint-image"
        , src "images/q.png"
        , title helpText
        ]
        []


viewFailureDatesChart : ( Posix, Posix ) -> List ( TestFailure, Color ) -> Html a
viewFailureDatesChart ( fromDate, toDate ) colorizedFailures =
    let
        leastTimestamp : Float
        leastTimestamp =
            toFloat <| Time.posixToMillis fromDate

        biggestTimestamp : Float
        biggestTimestamp =
            toFloat <| Time.posixToMillis toDate

        timestampRange : Float
        timestampRange =
            biggestTimestamp - leastTimestamp

        relativePosition : Int -> Float
        relativePosition t =
            100 * (toFloat t - leastTimestamp) / timestampRange
    in
    List.map
        (\( failure, color ) ->
            div
                [ class "timeline-dot"
                , style "background-color" (colorToHtml color)
                , style "left" (String.fromFloat (relativePosition (Time.posixToMillis failure.date)) ++ "%")
                ]
                [ text <| "   " ++ formatDateTime failure.date ]
        )
        colorizedFailures
        |> div [ class "timeline" ]


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    Maybe.withDefault [] << Dict.get classAndMethod


failuresTable : List ( TestFailure, Color ) -> Html Msg
failuresTable colorizedFailures =
    table [] (failuresTableHeaderRow :: List.map failureRow colorizedFailures)


failuresTableHeaderRow : Html a
failuresTableHeaderRow =
    tr []
        [ th [] [ text "Failed on" ]
        , th [] [ text "Build URL", helpIcon "Some build URLs are no longer available, because archived jobs are deleted after some time (usually a week)" ]
        , th [] [ text "Unique Stack Trace" ]
        , th [] [ text "Action" ]
        ]


failureRow : ( TestFailure, Color ) -> Html Msg
failureRow ( { url, date, stackTrace }, color ) =
    let
        buildLinkOrNA =
            if String.isEmpty url then
                text "N/A"

            else
                a [ href url ] [ text <| extractJobNameAndBuildNumber url ]
    in
    tr []
        [ td [] [ text <| formatDateTime date ]
        , td [] [ buildLinkOrNA ]
        , td [ style "background-color" (colorToHtml color) ] []
        , td [] [ button [ onClick (ShowStackTrace stackTrace) ] [ text "Show Stack Trace" ] ]
        ]


{-| From URL like "<https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/job/drools-downstream-pullrequests/12/testReport">
Extract "drools-downstream-pullrequests/12"
-}
extractJobNameAndBuildNumber : String -> String
extractJobNameAndBuildNumber fullUrl =
    String.split "/" fullUrl
        |> List.Extra.takeWhileRight (\piece -> piece /= "job")
        |> List.take 2
        |> String.join "/"


{-| YYYY-MM-DD HH:mm
-}
formatDateTime : Posix -> String
formatDateTime posix =
    let
        pad =
            String.padLeft 2 '0' << String.fromInt
    in
    formatDate posix
        ++ " "
        ++ pad (Time.toHour Time.utc posix)
        ++ ":"
        ++ pad (Time.toHour Time.utc posix)


{-| YYYY-MM-DD
-}
formatDate : Posix -> String
formatDate posix =
    String.join "-"
        [ Time.toYear Time.utc posix |> String.fromInt
        , Time.toMonth Time.utc posix |> monthNumber
        , Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
        ]


monthNumber : Month -> String
monthNumber month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


daysSinceLastFailure : List TestFailure -> Posix -> Int
daysSinceLastFailure failures posixNow =
    let
        daysFromNow posixPast =
            Time.posixToMillis posixNow - Time.posixToMillis posixPast |> toFloat |> millisToDays |> round
    in
    List.Extra.maximumBy (.date >> Time.posixToMillis) failures
        |> Maybe.map (\lastFailure -> daysFromNow lastFailure.date)
        |> Maybe.withDefault 0


fqnToSimpleClassName : String -> String
fqnToSimpleClassName fqn =
    String.split "." fqn |> List.reverse |> List.head |> Maybe.withDefault fqn


failureListDecoder : Decode.Decoder (List TestFailure)
failureListDecoder =
    Decode.list testFailureDecoder


standardDeviation : List Posix -> Float
standardDeviation dts =
    let
        ts : List Int
        ts =
            List.map Time.posixToMillis dts

        len : Float
        len =
            toFloat <| List.length ts

        avg : Float
        avg =
            toFloat (List.sum ts) / len

        summedSquares : Float
        summedSquares =
            List.sum <| List.map (\x -> (toFloat x - avg) ^ 2) ts
    in
    millisToDays <| sqrt <| summedSquares / len


millisToDays : Float -> Float
millisToDays millis =
    millis / (24 * 60 * 60 * 1000)


groupFailuresByClassAndMethod : List TestFailure -> GroupedFailures
groupFailuresByClassAndMethod =
    Dict.Extra.groupBy (\failure -> ( failure.testClass, failure.testMethod ))
        -- sort by failure date from oldest to most recent
        >> Dict.map (\_ failures -> List.sortBy (Time.posixToMillis << .date) failures)


testFailureDecoder : Decode.Decoder TestFailure
testFailureDecoder =
    Decode.map5 TestFailure
        (Decode.field "url" Decode.string)
        (Decode.field "date" Iso8601.decoder)
        (Decode.field "testClass" Decode.string)
        (Decode.field "testMethod" Decode.string)
        (Decode.field "stackTrace" Decode.string)


epoch : Posix
epoch =
    Time.millisToPosix 0


colorToHtml : Color -> String
colorToHtml color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    "RGB("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"


showHttpError : Http.Error -> String
showHttpError error =
    "Http Error : "
        ++ (case error of
                BadUrl x ->
                    "Bad Url : " ++ x

                Timeout ->
                    "Time"

                NetworkError ->
                    "NetworkError"

                BadStatus { url, status } ->
                    "BadStatus { code = " ++ String.fromInt status.code ++ ", message = " ++ status.message ++ ", url = " ++ url ++ " }"

                BadPayload _ _ ->
                    "BadPayload"
           )
