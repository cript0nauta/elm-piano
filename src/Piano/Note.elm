module Piano.Note
    exposing
        ( Note
        , naturalKeyDistance
        , noteName
        , isNatural
        , octave
        , allNotes
        )

{-| Type definitions and helpers for the Note type
-}

import String


{-| Represents a note giving its MIDI Note Number

See <http://www.electronics.dit.ie/staff/tscarff/Music_technology/midi/midi_note_numbers_for_octaves.htm> for more information

-}
type alias Note =
    Int


{-| A list with all valid MIDI notes
-}
allNotes : List Note
allNotes =
    List.range 0 127


{-| Represent a note number as a string
-}
noteName : Note -> String
noteName note =
    let
        getCharAt n str =
            String.slice n (n + 1) str

        noteName_ =
            getCharAt (note % 12) "CCDDEFFGGAAB"

        alteration =
            if isNatural note then
                ""
            else
                "#"
    in
        noteName_ ++ alteration ++ toString (octave note)


{-| Octave number of a note
-}
octave : Note -> Int
octave note =
    note // 12


{-| Return False if note is a flat or a sharp, True otherwise
-}
isNatural : Note -> Bool
isNatural note =
    List.member (note % 12) [ 0, 2, 4, 5, 7, 9, 11 ]


naturalKeyDistance : Note -> Note -> Int
naturalKeyDistance a b =
    case (compare a b) of
        EQ ->
            0

        GT ->
            List.range b a
                |> List.filter isNatural
                |> List.length
                |> (\n -> n - 1)

        LT ->
            List.range a b
                |> List.filter isNatural
                |> List.length
                |> (\n -> n - 1)
                |> negate
