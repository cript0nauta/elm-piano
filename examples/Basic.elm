module Basic exposing (..)

import Html exposing (..)
import Color
import Set
import Piano


main : Html msg
main =
    view model



-- MODEL


type alias Model =
    { pianoState : Piano.State
    }


model : Model
model =
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

        notes =
            Piano.allNotes
                |> List.filter shouldDisplayNote
                |> Set.fromList
    in
        Model
            (Piano.initialState
                |> Piano.setNotes notes
            )



-- VIEW


view : Model -> Html msg
view { pianoState } =
    let
        pianoConfig : Piano.Config msg
        pianoConfig =
            Piano.makeConfig Piano.keyboard88Keys
                |> Piano.colorAllPressedKeys Color.lightOrange Color.darkOrange
    in
        Piano.view pianoConfig pianoState
