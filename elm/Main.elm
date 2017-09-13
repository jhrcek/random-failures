module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Table
import Time
import Time.DateTime as DateTime exposing (DateTime, zero)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


initialModel : Model
initialModel =
    { groupedFailures = groupFailuresByClassAndMethod <| parseFailuresJson failureData
    , failureCountFilter = 2
    , tableState = Table.initialSort "# failures"
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


type Msg
    = ChangeFailureCountFilter String
    | SetTableState Table.State


type alias Model =
    { groupedFailures : GroupedFailures
    , failureCountFilter : Int
    , tableState : Table.State
    }


type alias GroupedFailures =
    Dict ( String, String ) (List TestFailure)


type alias TableRecord =
    ( ( String, String ), List TestFailure )


tableConfig : Table.Config TableRecord Msg
tableConfig =
    Table.config
        { toId = \( ( cl, m ), fs ) -> cl
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Class" (\( ( cl, m ), fs ) -> cl)
            , Table.stringColumn "Method" (\( ( cl, m ), fs ) -> m)
            , Table.intColumn "Failures" (\( ( cl, m ), fs ) -> List.length fs)
            , Table.floatColumn "Std dev of failure dates (in days)" (\( ( cl, m ), fs ) -> standardDeviation <| List.map .date fs)
            ]
        }


view : Model -> Html Msg
view model =
    div []
        [ filterControls model.failureCountFilter
        , failuresTable model
        ]


fqnToSimpleClassName : String -> String
fqnToSimpleClassName fqn =
    String.split "." fqn |> List.reverse |> List.head |> Maybe.withDefault fqn


filterControls : Int -> Html Msg
filterControls failureCountFilter =
    div []
        [ text "Show tests that failed "
        , input [ type_ "number", onInput ChangeFailureCountFilter, value (toString failureCountFilter) ] []
        , text " or more times"
        ]


failuresTable : Model -> Html Msg
failuresTable model =
    let
        acceptedFailures =
            Dict.toList model.groupedFailures
                |> List.filter (\( _, failures ) -> List.length failures >= model.failureCountFilter)
    in
    Table.view tableConfig model.tableState acceptedFailures


type alias TestFailure =
    { url : String
    , date : DateTime
    , testClass : String
    , testMethod : String
    , stackTrace : String
    }


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
    Dict.Extra.groupBy (\failure -> ( fqnToSimpleClassName failure.testClass, failure.testMethod ))


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


failureData : String
failureData =
    "[{\"url\":\"https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/job/errai-pullrequests/93/testReport/\",\"date\":[2017,8,15,19,34],\"testClass\":\"org.jboss.errai.ui.test.binding.client.BindingTemplateTest\",\"testMethod\":\"testBindingToJsTypeInterfaceWithJsOverlayHasValue\",\"stackTrace\":\"com.google.gwt.core.ext.UnableToCompleteException: (see previous log entries)\\n\"}]"
