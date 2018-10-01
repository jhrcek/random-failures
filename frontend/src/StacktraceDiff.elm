module StacktraceDiff exposing (diffView)

import Diff exposing (Change(..))
import Element as El exposing (Color, Element)
import Element.Background as Background
import TestFailure exposing (StackTrace, stackTraceToString)


diffView : Int -> StackTrace -> StackTrace -> Element msg
diffView modalWidth stackTrace1 stackTrace2 =
    El.table []
        { data =
            smashDiffs <|
                Diff.diffLines
                    (addNbsp stackTrace1)
                    (addNbsp stackTrace2)
        , columns =
            [ { header = El.none
              , width = El.px <| modalWidth // 2
              , view =
                    \smashedChange ->
                        case smashedChange of
                            RemAdd r _ ->
                                El.paragraph
                                    [ Background.color pink ]
                                    [ El.text <| "-\u{00A0}" ++ r ]

                            RemBlank r ->
                                El.paragraph
                                    [ Background.color pink ]
                                    [ El.text <| "-\u{00A0}" ++ r ]

                            BlankAdd _ ->
                                El.text "\u{00A0}"

                            Unchanged line ->
                                El.paragraph
                                    []
                                    [ El.text <| "  " ++ line ]
              }
            , { header = El.none
              , width = El.px <| modalWidth // 2
              , view =
                    \smashedChange ->
                        case smashedChange of
                            RemAdd _ a ->
                                El.paragraph
                                    [ Background.color green ]
                                    [ El.text <| "+\u{00A0}" ++ a ]

                            RemBlank _ ->
                                El.text "\u{00A0}"

                            BlankAdd a ->
                                El.paragraph
                                    [ Background.color green ]
                                    [ El.text <| "+\u{00A0}" ++ a ]

                            Unchanged line ->
                                El.paragraph
                                    []
                                    [ El.text <| "  " ++ line ]
              }
            ]
        }


{-| Translate output of Diff.diffLines so that the additions / removals are side by side
Example
INPUT: [Added "1", Added "2", Removed "3", NoChange "4", Added "5"]
OUTPUT: [RemAdd "1" "3", AddBlank "2", Same "4", BlankAdd "5"]
-}
smashDiffs : List (Change String) -> List SmashedChange
smashDiffs difs =
    let
        f change ( removals, additions, smashed ) =
            case change of
                Added a ->
                    ( removals, a :: additions, smashed )

                Removed r ->
                    ( r :: removals, additions, smashed )

                NoChange x ->
                    ( [], [], smashed ++ smash (List.reverse removals) (List.reverse additions) ++ [ Unchanged x ] )

        smash : List String -> List String -> List SmashedChange
        smash rms ads =
            case ( rms, ads ) of
                ( [], [] ) ->
                    []

                ( [], a :: ads_ ) ->
                    BlankAdd a :: smash [] ads_

                ( r :: rms_, [] ) ->
                    RemBlank r :: smash rms_ []

                ( r :: rms_, a :: ads_ ) ->
                    RemAdd r a :: smash rms_ ads_

        ( rems, adds, smashed_ ) =
            List.foldl f ( [], [], [] ) difs
    in
    smashed_ ++ smash rems adds


type SmashedChange
    = RemAdd String String
    | RemBlank String
    | BlankAdd String
    | Unchanged String


green : Color
green =
    El.rgb255 205 255 216


pink : Color
pink =
    El.rgb255 253 174 183



--| Prevent stupid looking breaks after "at"


addNbsp : StackTrace -> String
addNbsp =
    String.replace "at " "at\u{00A0}" << stackTraceToString
