module TestFailure exposing
    ( ClassAndMethod
    , GroupedFailures
    , StackTrace
    , StackTraceMode(..)
    , TestFailure
    , assignColorsToStacktraces
    , daysSinceLastFailure
    , extractJobNameAndBuildNumber
    , getStackTrace
    , groupByClassAndMethod
    , listDecoder
    , stackTraceToString
    , standardDeviation
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Dict.Extra
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Time exposing (Posix)


type alias TestFailure =
    { url : String
    , date : Posix
    , testClass : String
    , testMethod : String
    , stackTrace : StackTrace
    }


type alias GroupedFailures =
    Dict ClassAndMethod (List TestFailure)


type StackTrace
    = StackTrace String


type alias ClassAndMethod =
    ( String, String )


stackTraceToString : StackTrace -> String
stackTraceToString (StackTrace st) =
    st


getStackTrace : TestFailure -> String
getStackTrace =
    .stackTrace >> stackTraceToString


type StackTraceMode
    = NoStackTrace
    | ShowStackTrace StackTrace
    | OneStackTracePicked ( StackTrace, Color )
    | TwoStackTracesPicked ( StackTrace, Color ) ( StackTrace, Color )


listDecoder : Decoder (List TestFailure)
listDecoder =
    Decode.list decoder


decoder : Decoder TestFailure
decoder =
    Decode.map5 TestFailure
        (Decode.field "url" Decode.string)
        (Decode.field "date" Iso8601.decoder)
        (Decode.field "testClass" Decode.string)
        (Decode.field "testMethod" Decode.string)
        (Decode.map StackTrace <| Decode.field "stackTrace" Decode.string)


{-| From URL like "<https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/job/drools-downstream-pullrequests/12/testReport">
Extract "drools-downstream-pullrequests/12"
-}
extractJobNameAndBuildNumber : String -> String
extractJobNameAndBuildNumber fullUrl =
    String.split "/" fullUrl
        |> List.Extra.takeWhileRight (\piece -> piece /= "job")
        |> List.take 2
        |> String.join "/"


daysSinceLastFailure : List TestFailure -> Posix -> Int
daysSinceLastFailure failures posixNow =
    let
        daysFromNow posixPast =
            Time.posixToMillis posixNow - Time.posixToMillis posixPast |> toFloat |> millisToDays |> round
    in
    List.Extra.maximumBy (.date >> Time.posixToMillis) failures
        |> Maybe.map (\lastFailure -> daysFromNow lastFailure.date)
        |> Maybe.withDefault 0


millisToDays : Float -> Float
millisToDays millis =
    millis / (24 * 60 * 60 * 1000)


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


assignColorsToStacktraces : List TestFailure -> List ( TestFailure, Color )
assignColorsToStacktraces failures =
    let
        stacktraceToColor : Dict String Color
        stacktraceToColor =
            List.map getStackTrace failures
                |> List.Extra.unique
                |> (\uniqueStacktraces -> List.map2 (\st color -> ( st, color )) uniqueStacktraces stacktraceColors)
                |> Dict.fromList

        assignColor : TestFailure -> Color
        assignColor f =
            Dict.get (getStackTrace f) stacktraceToColor |> Maybe.withDefault Color.white
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


groupByClassAndMethod : List TestFailure -> GroupedFailures
groupByClassAndMethod =
    Dict.Extra.groupBy (\failure -> ( failure.testClass, failure.testMethod ))
        -- sort by failure date from oldest to most recent
        >> Dict.map (\_ failures -> List.sortBy (Time.posixToMillis << .date) failures)
