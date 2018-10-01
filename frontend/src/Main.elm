module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Color exposing (Color)
import Dict
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, div, text)
import Html.Attributes as Attr exposing (class, href, maxlength, style, target, title, type_)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
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
    Http.get "failures.json" TestFailure.listDecoder
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
                    El.text "Request to load failures.json wasn't sent"

                Loading ->
                    El.text "Loading data ..."

                Failure httpError ->
                    El.text <| showHttpError httpError

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
    { title = "kiegroup CI Test Failures", body = [ El.layout [ Font.family [ Font.typeface "calibri", Font.typeface "helvetica" ] ] body ] }


homeView : Model -> Element Msg
homeView model =
    El.column []
        [ description model.filters.dateRange
        , filterControls model
        , El.el [ Region.heading 3 ] (El.text "Failures (grouped by Class and Test method)")
        , failureSummaryTable model
        , faq
        ]


classDetailsView : String -> Model -> Element Msg
classDetailsView fqcn model =
    El.column []
        [ homeLink
        , El.el [ Region.heading 3 ] (El.text <| "Failures in class " ++ fqcn)
        , failureSummaryTable model
        ]


classAndMethodNotFoundView : ClassAndMethod -> Element Msg
classAndMethodNotFoundView ( clz, method ) =
    let
        boldText =
            El.el [ Font.bold, El.width (El.px 100) ] << El.text
    in
    El.column []
        [ homeLink
        , El.el [ Region.heading 3 ] (El.text <| "No failures found")
        , El.row [] [ boldText "Class", El.text clz ]
        , El.row [] [ boldText "Method", El.text method ]
        , El.text "Are you sure you've got the class and method name right?"
        ]


description : ( Posix, Posix ) -> Element Msg
description ( fromDate, toDate ) =
    El.column []
        [ El.el [ Region.heading 2 ] (El.text "Random test failure analysis")
        , El.paragraph []
            [ El.text "This report was generated on GENERATED_ON_PLACEHOLDER and lists most"
            , helpIcon "Builds with more than 50 test failures are excluded, because they add a lot of data without providing much value for flaky test identification."
            , El.text "test failures in "
            , El.link []
                { url = "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/"
                , label = El.text "rhba-jenkins PR jobs"
                }
            , El.text <| " (master only) from " ++ formatDate fromDate ++ " to " ++ formatDate toDate ++ "."
            ]
        ]


filterControls : { a | filters : Filters } -> Element Msg
filterControls { filters } =
    El.paragraph []
        [ El.text "Show tests that failed "
        , Input.text [ El.htmlAttribute (type_ "number"), El.htmlAttribute (maxlength 2), El.htmlAttribute (Attr.min "0"), El.htmlAttribute (Attr.max "100"), El.htmlAttribute (style "width" "50px") ]
            { onChange = ChangeFailureCountFilter
            , text = String.fromInt filters.failureCount
            , placeholder = Nothing
            , label = Input.labelLeft [] (El.text " or more times")
            }
        ]


failureSummaryTable : Model -> Element Msg
failureSummaryTable model =
    case model.groupedFailures of
        Success failureData ->
            let
                acceptedFailures =
                    applyFilters model.filters failureData
            in
            El.column []
                [ El.html <| Table.view (tableConfig model.now) model.tableState acceptedFailures
                , El.text "* Standard deviation of failure dates (in days)"
                ]

        _ ->
            El.text "No data available"


applyFilters : Filters -> GroupedFailures -> List ( ClassAndMethod, List TestFailure )
applyFilters filters groupedFailures =
    Dict.toList groupedFailures
        |> List.filter (\( _, failures ) -> filters.failureCount <= List.length failures)
        |> List.filter (\( ( fqcn, _ ), _ ) -> filters.className |> Maybe.map (\chosenFqcn -> chosenFqcn == fqcn) |> Maybe.withDefault True)


faq : Element Msg
faq =
    El.column []
        [ El.el [ Region.heading 2 ] (El.text "When is test method considered to be failing randomly?")
        , El.paragraph []
            [ El.text <|
                "The following heuristic is used to highlight random failures. "
                    ++ "A test is considered randomly failing if all of the following conditions hold (open to discussion!):"
            ]
        , El.text "Failed 5 or more times"
        , El.text "Last failure ocurred no longer than 14 days ago"
        , El.text "Standard deviation of failure dates is greater than 5 days"
        , El.el [ Region.heading 2 ] (El.text "Got ideas about how to make this report more useful?")
        , El.text "File an issue on "
        , El.link [] { url = "https://github.com/jhrcek/random-failures/issues", label = El.text "project's page" }
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
            , children = [ El.layout [] (gitHubLinkFromFailures failures) ]
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


gitHubLinkFromFailures : List TestFailure -> Element a
gitHubLinkFromFailures =
    withGitInfo gitHubSourceLink (El.text "N/A")


gitHubSourceLink : GitInfo -> Element a
gitHubSourceLink gitInfo =
    El.link [ El.htmlAttribute (target "_blank"), El.htmlAttribute (title "Class source on GitHub") ]
        { url = gitHubClassUrl gitInfo
        , label = El.text gitInfo.repo
        }


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


failureDetailView : ClassAndMethod -> List TestFailure -> ( Posix, Posix ) -> StackTraceMode -> ViewPort -> Element Msg
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
    El.column []
        [ homeLink
        , El.el [ Region.heading 2 ] (El.text "Failure details")
        , failureDetailsSummary classAndMethod
            gitHubLink
            (List.length sortedFailures)
            (List.length uniqueStacktracesAndMessages)
            (List.length uniqueStacktraces)
        , El.el [ Region.heading 3 ] (El.text "Spread of failure dates")
        , viewFailureDatesChart dateRange colorizedFailures
        , El.el [ Region.heading 3 ] (El.text "Failures")
        , failuresTable stackTraceMode colorizedFailures
        , stackTraceView maybeGitInfo classAndMethod stackTraceMode viewport
        ]


homeLink : Element Msg
homeLink =
    El.link []
        { url = Page.toUrlHash Page.Home
        , label = El.text "<< home"
        }


failureDetailsSummary : ClassAndMethod -> Element Msg -> Int -> Int -> Int -> Element Msg
failureDetailsSummary (( fqcn, _ ) as classAndMethod) gitHubLink totalFailures uniqueStacktracesAndMessagesCount uniqueStacktracesCount =
    let
        fistColumnWidth =
            El.width (El.px 500)

        boldText =
            El.el [ Font.bold, fistColumnWidth ] << El.text
    in
    El.column []
        [ El.row []
            [ boldText "Class"
            , El.link []
                { url = Page.toUrlHash <| Page.ClassDetails fqcn
                , label = El.text fqcn
                }
            ]
        , El.row []
            [ boldText "Method"
            , El.text <| getMethodName classAndMethod
            ]
        , El.row []
            [ boldText "Source code on GitHub"
            , gitHubLink
            ]
        , El.row []
            [ boldText "Total failures"
            , El.text <| String.fromInt totalFailures
            ]
        , El.row []
            [ El.paragraph [ fistColumnWidth ]
                [ El.el [ Font.bold ] (El.text "Unique stack traces (including ex. message)")
                , helpIcon <|
                    "Total number unique stack traces including exception message "
                        ++ "(looking at both WHERE the failure occured AND the exception message)"
                ]
            , El.text <| String.fromInt uniqueStacktracesAndMessagesCount
            ]
        , El.row []
            [ El.paragraph [ fistColumnWidth ]
                [ El.el [ Font.bold ] (El.text "Unique stack traces")
                , helpIcon <|
                    "Total number unique stack traces that are different disregarding exception message "
                        ++ "(just looking at WHERE the failure was, ignoring exception message)"
                ]
            , El.text <| String.fromInt uniqueStacktracesCount
            ]
        ]


stackTraceView : Maybe GitInfo -> ClassAndMethod -> StackTraceMode -> ViewPort -> Element Msg
stackTraceView maybeGitInfo ( fqcn, _ ) stackTraceMode viewport =
    case stackTraceMode of
        NoStackTrace ->
            El.none

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
            El.none

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


stackTraceModal : ViewPort -> El.Element Msg -> El.Element Msg -> Element Msg
stackTraceModal viewport title body =
    El.el
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


helpIcon : String -> Element a
helpIcon helpText =
    El.image
        [ El.width (El.px 15)
        , El.height (El.px 15)
        ]
        { src = "images/q.png"
        , description = helpText
        }



-- TODO migrate viewFailureDatesChart to Element


viewFailureDatesChart : ( Posix, Posix ) -> List ( TestFailure, Color ) -> Element a
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
        |> El.html


getSortedFailuresOf : ClassAndMethod -> GroupedFailures -> List TestFailure
getSortedFailuresOf classAndMethod =
    Maybe.withDefault [] << Dict.get classAndMethod


failuresTable : StackTraceMode -> List ( TestFailure, Color ) -> Element Msg
failuresTable stackTraceMode colorizedFailures =
    El.table []
        { data = colorizedFailures
        , columns =
            [ { header = El.text "Failed on"
              , width = El.px 100
              , view = \( { date }, _ ) -> El.text <| formatDateTime date
              }
            , { header =
                    El.row []
                        [ El.text "Build URL"
                        , helpIcon "Some build URLs are no longer available, because archived jobs are deleted after some time (usually a week)"
                        ]
              , width = El.px 100
              , view =
                    \( { url }, _ ) ->
                        if String.isEmpty url then
                            El.text "N/A"

                        else
                            El.link [] { url = url, label = El.text <| TestFailure.extractJobNameAndBuildNumber url }
              }
            , { header = El.text "Unique Stack Trace"
              , width = El.px 100
              , view =
                    \( { stackTrace }, color ) ->
                        El.el [ Background.color (translateColor color) ]
                            (if isSelectedForComparison stackTraceMode stackTrace then
                                El.text " "

                             else
                                El.text "Selected for comparison"
                            )
              }
            , { header = El.text "Stack Trace"
              , width = El.px 100
              , view =
                    \( { stackTrace }, color ) ->
                        let
                            ( newStackTraceMode, comparisonButtonLabel ) =
                                case stackTraceMode of
                                    NoStackTrace ->
                                        ( OneStackTracePicked ( stackTrace, color )
                                        , "Compare"
                                        )

                                    OneStackTracePicked ( stackTrace1, color1 ) ->
                                        if stackTrace1 == stackTrace then
                                            --This one already pick -> unpick it
                                            ( NoStackTrace
                                            , "Unselect"
                                            )

                                        else
                                            ( TwoStackTracesPicked ( stackTrace1, color1 ) ( stackTrace, color )
                                            , "Compare with selected"
                                            )

                                    TwoStackTracesPicked _ _ ->
                                        ( OneStackTracePicked ( stackTrace, color )
                                        , "Compare"
                                        )

                                    ShowStackTrace _ ->
                                        ( OneStackTracePicked ( stackTrace, color )
                                        , "Compare"
                                        )
                        in
                        El.row []
                            [ Input.button []
                                { onPress = Just <| SetStackTraceMode <| ShowStackTrace stackTrace
                                , label = El.text "Show"
                                }
                            , Input.button []
                                { onPress = Just <| SetStackTraceMode newStackTraceMode
                                , label = El.text comparisonButtonLabel
                                }
                            ]
              }
            ]
        }


isSelectedForComparison : StackTraceMode -> StackTrace -> Bool
isSelectedForComparison stackTraceMode stackTrace =
    case stackTraceMode of
        OneStackTracePicked ( stackTrace1, _ ) ->
            stackTrace1 == stackTrace

        NoStackTrace ->
            False

        TwoStackTracesPicked _ _ ->
            False

        ShowStackTrace _ ->
            False


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

                BadStatus { url, status } ->
                    "BadStatus { code = " ++ String.fromInt status.code ++ ", message = " ++ status.message ++ ", url = " ++ url ++ " }"

                BadPayload _ _ ->
                    "BadPayload"
           )
