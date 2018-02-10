module Utils exposing (..)

import Set
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Piano exposing (..)
import Piano.Note exposing (..)


sizeSelector : (( Note, Note ) -> msg) -> Html msg
sizeSelector msg =
    let
        keyboardSizeOption size =
            let
                keys =
                    Tuple.second size - Tuple.first size + 1
            in
                button [ onClick (msg size) ]
                    [ text (toString keys ++ "-key piano") ]
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
        [ style [ ( "text-align", "center" ) ] ]
        [ text <|
            "Currently pressed notes: "
                ++ String.join ", "
                    (getNotes state |> Set.toList |> List.map noteName)
        ]
