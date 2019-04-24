module Utils exposing (debugNotes, sizeSelector)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Piano exposing (..)
import Set


sizeSelector : (( Note, Note ) -> msg) -> Html msg
sizeSelector msg =
    let
        keyboardSizeOption size =
            button [ onClick (msg size) ]
                [ text (String.fromInt (numberOfNotes size) ++ "-key piano") ]

        numberOfNotes : ( Note, Note ) -> Int
        numberOfNotes ( first, second ) =
            second - first + 1
    in
    List.map keyboardSizeOption
        [ keyboard12Keys
        , keyboard25Keys
        , keyboard49Keys
        , keyboard61Keys
        , keyboard76Keys
        , keyboard88Keys
        ]
        |> List.intersperse (br [] [])
        |> div []


debugNotes : State -> Html msg
debugNotes state =
    div
        [ style "text-align" "center" ]
        [ text <|
            "Currently pressed notes: "
                ++ String.join ", "
                    (getNotes state |> Set.toList |> List.map noteName)
        ]
