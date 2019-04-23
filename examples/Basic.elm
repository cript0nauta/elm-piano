module Basic exposing (main, notes)

import Color
import Html exposing (..)
import Piano
import Set


notes : Set.Set Piano.Note
notes =
    Piano.allNotes
        |> List.filter shouldDisplayNote
        |> Set.fromList


{-| Show the notes D, F#, A in all the octaves
-}
shouldDisplayNote : Piano.Note -> Bool
shouldDisplayNote note =
    List.member
        (remainderBy 12 note)
        dMajorKeys


{-| D, F#, A
-}
dMajorKeys =
    [ 2, 6, 9 ]



-- VIEW


main : Html Never
main =
    Piano.viewStatic pianoConfig notes


pianoConfig : Piano.Config
pianoConfig =
    Piano.makeConfig Piano.keyboard88Keys
        |> Piano.colorAllPressedKeys
            (Color.toRgb Color.lightOrange)
            (Color.toRgb Color.darkOrange)
