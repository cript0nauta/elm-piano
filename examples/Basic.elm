module Basic exposing (main, notes)

import Color
import Html exposing (..)
import Piano
import Set


notes : Set.Set Piano.Note
notes =
    let
        -- D, F#, A
        dMajorKeys =
            [ 2, 6, 9 ]

        -- Show the notes D, F#, A in all the octaves
        shouldDisplayNote : Piano.Note -> Bool
        shouldDisplayNote note =
            List.member
                (remainderBy 12 note)
                dMajorKeys
    in
    Piano.allNotes
        |> List.filter shouldDisplayNote
        |> Set.fromList



-- VIEW


main : Html Never
main =
    let
        pianoConfig : Piano.Config
        pianoConfig =
            Piano.makeConfig Piano.keyboard88Keys
                |> Piano.colorAllPressedKeys Color.lightOrange Color.darkOrange
    in
    Piano.viewStatic pianoConfig notes
