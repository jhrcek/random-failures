module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Color exposing (Color)
import Dict
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, h2, h3, img, input, li, strong, table, td, text, th, tr, ul)
import Html.Attributes as Attr exposing (class, href, maxlength, src, style, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..), expectJson)
import List.Extra
import Page exposing (Page)
import Regex
import RemoteData exposing (RemoteData(..), WebData)
import StacktraceDiff
import Table exposing (defaultCustomizations)
import Task
import TestFailure exposing (ClassAndMethod, GitInfo, GroupedFailures, StackTrace(..), StackTraceMode(..), TestFailure, getStackTrace, stackTraceToString)
import Time exposing (Month(..), Posix)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> SetViewPort (ViewPort (toFloat w) (toFloat h)))


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
        , Task.perform SetDomViewPort Browser.Dom.getViewport
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
    Http.get
        { url = "failures.json"
        , expect = expectJson (FailuresLoaded << RemoteData.fromResult) TestFailure.listDecoder
        }


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
    , navKey = key
    , viewport = initialViewPort
    }


initialViewPort : ViewPort
initialViewPort =
    { width = 1920
    , height = 1080
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

        SetStackTraceMode mode ->
            ( setStackTrace mode model
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
                | groupedFailures = RemoteData.map TestFailure.groupByClassAndMethod failuresRemoteData
                , filters = setDateRangeFilter dateRange model.filters
              }
            , Cmd.none
            )

        SetDomViewPort domViewPort ->
            ( { model
                | viewport =
                    { height = domViewPort.viewport.height
                    , width = domViewPort.viewport.width
                    }
              }
            , Cmd.none
            )

        SetViewPort wp ->
            ( { model | viewport = wp }, Cmd.none )


setStackTrace : StackTraceMode -> Model -> Model
setStackTrace stackTraceMode model =
    case model.page of
        Page.MethodDetails classAndMethod _ ->
            { model | page = Page.MethodDetails classAndMethod stackTraceMode }

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
    | SetStackTraceMode StackTraceMode
    | UrlChange Url
    | LinkClicked UrlRequest
    | FailuresLoaded (WebData (List TestFailure))
    | SetDomViewPort Browser.Dom.Viewport
    | SetViewPort ViewPort


type alias Model =
    { groupedFailures : WebData GroupedFailures
    , filters : Filters
    , tableState : Table.State
    , page : Page
    , now : Posix
    , navKey : Key
    , viewport : ViewPort
    }


type alias TableRecord =
    ( ClassAndMethod, List TestFailure )


type alias ViewPort =
    { width : Float
    , height : Float
    }


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

                        Page.MethodDetails classAndMethod stackTraceMode ->
                            let
                                failures =
                                    getSortedFailuresOf classAndMethod loadedFailures
                            in
                            if List.isEmpty failures then
                                classAndMethodNotFoundView classAndMethod

                            else
                                failureDetailView classAndMethod failures model.filters.dateRange stackTraceMode model.viewport
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
        , text "This report lists most"
        , helpIcon "Builds with more than 50 test failures are excluded, because they add a lot of data without providing much value for flaky test identification."
        , text "test failures in "
        , a [ href "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/" ] [ text "rhba-jenkins PR jobs" ]
        , text <| " (master only) that occurred from " ++ formatDate fromDate ++ " to " ++ formatDate toDate ++ "."
        ]


filterControls : { a | filters : Filters } -> Html Msg
filterControls { filters } =
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
                [ Table.view (tableConfig model.now) model.tableState acceptedFailures
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


tableConfig : Posix -> Table.Config TableRecord Msg
tableConfig now =
    Table.customConfig
        { toId = \( ( fqcn, method ), _ ) -> fqcn ++ method
        , toMsg = SetTableState
        , columns =
            [ gitHubLinkColumn
            , classColumn
            , methodColumn
            , Table.intColumn "Failures" (\( ( _, _ ), fs ) -> List.length fs)
            , stdDevColumn
            , Table.intColumn daysSinceLastFailureColumnName (\( _, fs ) -> TestFailure.daysSinceLastFailure fs now)
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
            TestFailure.daysSinceLastFailure fs now

        failureDatesStdDev =
            TestFailure.standardDeviation <| List.map .date fs

        failureCount =
            List.length fs
    in
    failureCount >= 5 && lastFailureDaysAgo < 14 && failureDatesStdDev > 5


gitHubLinkColumn : Table.Column TableRecord Msg
gitHubLinkColumn =
    let
        viewData ( _, failures ) =
            { attributes = []
            , children = [ gitHubLinkFromFailures failures ]
            }

        sortByRepoName ( _, failures ) =
            withGitInfo .repo "N/A" failures
    in
    Table.veryCustomColumn
        { name = "GitHub Link"
        , viewData = viewData
        , sorter = Table.increasingOrDecreasingBy sortByRepoName
        }


classColumn : Table.Column TableRecord Msg
classColumn =
    let
        getVisibleClassName ( ( fqcn, _ ), _ ) =
            fqnToSimpleClassName fqcn

        classDetailsLink (( ( fqcn, _ ), _ ) as record) =
            { attributes = []
            , children =
                [ a [ href <| Page.toUrlHash <| Page.ClassDetails fqcn ]
                    [ text <| getVisibleClassName record ]
                ]
            }
    in
    Table.veryCustomColumn
        { name = "Class"
        , sorter = Table.increasingOrDecreasingBy getVisibleClassName
        , viewData = classDetailsLink
        }


{-| If the first failure in the list has GitInfo apply the callback to it.
Otherwise return the 2nd argument.
-}
withGitInfo : (GitInfo -> r) -> r -> List TestFailure -> r
withGitInfo gitInfoCallback resultWhenGitInfoNotAvailable failures =
    List.head failures
        |> Maybe.andThen (\firstFailure -> Maybe.map gitInfoCallback firstFailure.gitInfo)
        |> Maybe.withDefault resultWhenGitInfoNotAvailable


gitHubLinkFromFailures : List TestFailure -> Html a
gitHubLinkFromFailures =
    withGitInfo gitHubSourceLink (text "N/A")


gitHubSourceLink : GitInfo -> Html a
gitHubSourceLink gitInfo =
    a [ href (gitHubClassUrl gitInfo), target "_blank", title "Class source on GitHub" ]
        [ text gitInfo.repo ]


gitHubClassUrl : GitInfo -> String
gitHubClassUrl { repo, pathInRepo } =
    "https://github.com/kiegroup/" ++ repo ++ "/blob/master/" ++ pathInRepo


gitHubClassUrlWithLine : GitInfo -> Int -> String
gitHubClassUrlWithLine gitInfo lineNumber =
    gitHubClassUrl gitInfo ++ "#L" ++ String.fromInt lineNumber


methodColumn : Table.Column TableRecord Msg
methodColumn =
    let
        detailsLink ( classAndMethod, _ ) =
            { attributes = []
            , children =
                [ a [ href (Page.toUrlHash (Page.MethodDetails classAndMethod NoStackTrace)) ]
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
            TestFailure.standardDeviation <| List.map .date fs
    in
    Table.customColumn
        { name = "Spread of failure dates *"
        , viewData = FormatNumber.format usLocale << getDeviation
        , sorter = Table.decreasingOrIncreasingBy getDeviation
        }


failureDetailView : ClassAndMethod -> List TestFailure -> ( Posix, Posix ) -> StackTraceMode -> ViewPort -> List (Html Msg)
failureDetailView classAndMethod sortedFailures dateRange stackTraceMode viewport =
    let
        stacktraces =
            List.map getStackTrace sortedFailures

        uniqueStacktracesAndMessages =
            List.Extra.unique stacktraces

        uniqueStacktraces =
            List.Extra.uniqueBy (\st -> String.split "\t" st |> List.tail |> Maybe.withDefault [] |> String.concat) stacktraces

        colorizedFailures =
            TestFailure.assignColorsToStacktraces sortedFailures

        gitHubLink =
            gitHubLinkFromFailures sortedFailures

        maybeGitInfo =
            withGitInfo Just Nothing sortedFailures
    in
    [ homeLink
    , h2 [] [ text "Failure details" ]
    , failureDetailsSummary classAndMethod
        gitHubLink
        (List.length sortedFailures)
        (List.length uniqueStacktracesAndMessages)
        (List.length uniqueStacktraces)
    , h3 [] [ text "Spread of failure dates" ]
    , viewFailureDatesChart dateRange colorizedFailures
    , h3 [] [ text "Failures" ]
    , failuresTable stackTraceMode colorizedFailures
    , stackTraceView maybeGitInfo classAndMethod stackTraceMode viewport
    ]


homeLink : Html Msg
homeLink =
    a [ href (Page.toUrlHash Page.Home) ] [ text "<< home" ]


failureDetailsSummary : ClassAndMethod -> Html Msg -> Int -> Int -> Int -> Html Msg
failureDetailsSummary (( fqcn, _ ) as classAndMethod) gitHubLink totalFailures uniqueStacktracesAndMessagesCount uniqueStacktracesCount =
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
            [ td [] [ strong [] [ text "Source code on GitHub" ] ]
            , td [] [ gitHubLink ]
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


stackTraceView : Maybe GitInfo -> ClassAndMethod -> StackTraceMode -> ViewPort -> Html Msg
stackTraceView maybeGitInfo ( fqcn, _ ) stackTraceMode viewport =
    case stackTraceMode of
        NoStackTrace ->
            text ""

        ShowStackTrace stackTrace ->
            stackTraceModal viewport
                (El.text "Stack Trace")
                (El.html
                    (maybeGitInfo
                        |> Maybe.map (\gitInfo -> addLineLinksToGitHub gitInfo fqcn stackTrace)
                        |> Maybe.withDefault (text <| stackTraceToString stackTrace)
                    )
                )

        OneStackTracePicked _ ->
            text ""

        TwoStackTracesPicked ( stackTrace1, color1 ) ( stackTrace2, color2 ) ->
            stackTraceModal viewport
                (El.row []
                    [ El.text "Stack Trace Comparison "
                    , colorBlock stackTrace1 color1
                    , El.text " vs "
                    , colorBlock stackTrace2 color2
                    ]
                )
                (StacktraceDiff.diffView
                    (modalWidth viewport)
                    stackTrace1
                    stackTrace2
                )


{-| If the stack trace contains direct references to the class name
e.g. "at org.drools.scorecards.ScoringStrategiesTest.executeAndFetchScore(ScoringStrategiesTest.java:221)"
We surround the "ScoringStrategiesTest.java:221" with direct link to that line of source code on GitHub
-}
addLineLinksToGitHub : GitInfo -> String -> StackTrace -> Html a
addLineLinksToGitHub gitInfo fqcn stackTrace =
    let
        st =
            stackTraceToString stackTrace

        classNameWithLineNumberRegex =
            Maybe.withDefault Regex.never <| Regex.fromString (fqnToSimpleClassName fqcn ++ "\\.java:\\d+")

        matches =
            Regex.find classNameWithLineNumberRegex st

        processMatch aMatch ( curIndex, htmlList ) =
            let
                textBeforeMatch =
                    String.slice curIndex aMatch.index st

                newIndex =
                    aMatch.index + String.length aMatch.match

                lineNumber =
                    String.split ":" aMatch.match
                        |> List.drop 1
                        |> List.head
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0
            in
            ( newIndex
            , Html.a [ href <| gitHubClassUrlWithLine gitInfo lineNumber, target "_blank" ] [ Html.text aMatch.match ]
                :: Html.text textBeforeMatch
                :: htmlList
            )
    in
    List.foldl processMatch ( 0, [] ) matches
        |> (\( idx, htmlList ) ->
                List.reverse <| Html.text (String.slice idx (String.length st) st) {- remaining piece of text after the last match -} :: htmlList
           )
        |> Html.div []


colorBlock : StackTrace -> Color -> El.Element Msg
colorBlock st c =
    El.el
        [ Background.color <| translateColor c
        , Events.onClick <| SetStackTraceMode <| ShowStackTrace st
        , El.width <| El.px 100
        , El.height <| El.px 20
        ]
        El.none


translateColor : Color -> El.Color
translateColor ccolor =
    let
        c =
            Color.toRgb ccolor
    in
    El.rgb255 c.red c.green c.blue


black : El.Color
black =
    El.rgb 0 0 0


gray : El.Color
gray =
    El.rgb 0.88 0.88 0.88


white : El.Color
white =
    El.rgb 1 1 1


stackTraceModal : ViewPort -> El.Element Msg -> El.Element Msg -> Html Msg
stackTraceModal viewport title body =
    El.layout
        [ El.inFront <|
            El.column
                [ El.centerX
                , El.centerY
                , El.padding 10
                , Background.color gray
                , Border.solid
                , Border.width 1
                , Border.color black
                , Font.family [ Font.typeface "calibri" ]
                , Font.size 20
                ]
                [ El.row [ El.width El.fill ]
                    [ title
                    , El.el
                        [ El.alignRight
                        , Events.onClick (SetStackTraceMode NoStackTrace)
                        , Font.size 30
                        ]
                        (El.text "×")
                    ]
                , El.el
                    [ Font.size 13
                    , Font.family [ Font.monospace ]
                    , El.width <| El.px <| modalWidth viewport
                    , El.height <| El.px <| modalHeight viewport
                    , El.htmlAttribute <| style "overflow-wrap" "break-word"
                    , El.htmlAttribute <| style "white-space" "pre-wrap"
                    , El.htmlAttribute <| style "word-break" "keep-all"
                    , Background.color white
                    , El.scrollbarY
                    ]
                    body
                ]
        ]
        El.none


modalWidth : ViewPort -> Int
modalWidth { width } =
    round <| 0.8 * width


modalHeight : ViewPort -> Int
modalHeight { height } =
    round <| 0.8 * height


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


failuresTable : StackTraceMode -> List ( TestFailure, Color ) -> Html Msg
failuresTable stackTraceMode colorizedFailures =
    table [] (failuresTableHeaderRow :: List.map (failureRow stackTraceMode) colorizedFailures)


failuresTableHeaderRow : Html a
failuresTableHeaderRow =
    tr []
        [ th [] [ text "Failed on" ]
        , th [] [ text "Build URL", helpIcon "Some build URLs are no longer available, because archived jobs are deleted after some time (usually a week)" ]
        , th [] [ text "Unique Stack Trace" ]
        , th [] [ text "Stack Trace" ]
        ]


failureRow : StackTraceMode -> ( TestFailure, Color ) -> Html Msg
failureRow stackTraceMode ( { url, date, stackTrace }, color ) =
    let
        buildLinkOrNA =
            if String.isEmpty url then
                text "N/A"

            else
                a [ href url ] [ text <| TestFailure.extractJobNameAndBuildNumber url ]

        ( newStackTraceMode, comparisonButtonLabel, isSelectedForComparison ) =
            case stackTraceMode of
                NoStackTrace ->
                    ( OneStackTracePicked ( stackTrace, color )
                    , "Compare"
                    , False
                    )

                OneStackTracePicked ( stackTrace1, color1 ) ->
                    if stackTrace1 == stackTrace then
                        --This one already pick -> unpick it
                        ( NoStackTrace
                        , "Unselect"
                        , True
                        )

                    else
                        ( TwoStackTracesPicked ( stackTrace1, color1 ) ( stackTrace, color )
                        , "Compare with selected"
                        , False
                        )

                TwoStackTracesPicked _ _ ->
                    ( OneStackTracePicked ( stackTrace, color )
                    , "Compare"
                    , False
                    )

                ShowStackTrace _ ->
                    ( OneStackTracePicked ( stackTrace, color )
                    , "Compare"
                    , False
                    )
    in
    tr []
        [ td [] [ text <| formatDateTime date ]
        , td [] [ buildLinkOrNA ]
        , td [ style "background-color" (colorToHtml color) ] <|
            if isSelectedForComparison then
                [ text "Selected for comparison" ]

            else
                []
        , td []
            [ button
                [ onClick <| SetStackTraceMode <| ShowStackTrace stackTrace ]
                [ text "Show" ]
            , button
                [ onClick <| SetStackTraceMode newStackTraceMode ]
                [ text comparisonButtonLabel ]
            ]
        ]


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


fqnToSimpleClassName : String -> String
fqnToSimpleClassName fqn =
    String.split "." fqn |> List.reverse |> List.head |> Maybe.withDefault fqn


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

                BadStatus statusCode ->
                    "BadStatus " ++ String.fromInt statusCode

                BadBody str ->
                    "BadBody " ++ str
           )
