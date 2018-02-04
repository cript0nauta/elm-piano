module Utils exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Piano exposing (..)


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
