module Main exposing (..)

import Html exposing (Html, button, div, text)
import Json.Decode as Decode exposing (..)


main : Html msg
main =
    text <| toString <| Result.map proc <| Decode.decodeString (list testFailureDecoder) data


type alias TestFailure =
    { url : String
    , date : List Int
    , testClass : String
    , testMethod : String
    , stackTrace : String
    }


proc : List TestFailure -> String
proc =
    String.join "\n" << List.map (\{ date } -> toString date)


testFailureDecoder : Decoder TestFailure
testFailureDecoder =
    Decode.map5 TestFailure
        (field "url" string)
        (field "date" (list int))
        (field "testClass" string)
        (field "testMethod" string)
        (field "stackTrace" string)


data : String
data =
    "[{\"url\":\"https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/job/errai-pullrequests/93/testReport/\",\"date\":[2017,8,15,19,34],\"testClass\":\"org.jboss.errai.ui.test.binding.client.BindingTemplateTest\",\"testMethod\":\"testBindingToJsTypeInterfaceWithJsOverlayHasValue\",\"stackTrace\":\"com.google.gwt.core.ext.UnableToCompleteException: (see previous log entries)\\n\"}]"
