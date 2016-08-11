module Piano exposing (
    Model, Msg, Note, initialModel, isNatural, keyboard12Keys,
    keyboard25Keys, keyboard49Keys, keyboard61Keys, keyboard76Keys,
    keyboard88Keys, noteName, octave, update, view)

{-| A customizable piano component

# Model
@docs Model
@docs initialModel
@docs Note

# Messages and updates
@docs update
@docs Msg

# Keyboard size helpers
@docs keyboard12Keys
@docs keyboard25Keys
@docs keyboard49Keys
@docs keyboard61Keys
@docs keyboard76Keys
@docs keyboard88Keys

# HTML rendering
@docs view

# Note helpers
@docs noteName
@docs isNatural
@docs octave

-}

import Html exposing (..)
-- import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set
import String
import Piano.PianoStyle exposing (css)


-- main =
--     App.beginnerProgram
--         { model = initialModel
--         , update = update
--         , view = view
--         }


-- MODEL

{-| The model of the component.

notes is the set of currently pressed notes.

noteRange determines the first and last notes of the keyboard.

If interactive is True, the component will generate KeyUp and KeyDown messages
when the user clicks on a note. (Now this mode is experimental and has some
UI issues).

If showSizeSelector is True a button group will be shown to select the keyboard
size.

If debugNotes is True a text will appear, showin the note names of each
currently pressed note.
-}
type alias Model = 
    { notes: Set.Set Note
    , noteRange: (Note, Note)
    , interactive: Bool
    , showSizeSelector: Bool
    , debugNotes: Bool
    }

{-| Represents a note giving its MIDI Note Number

See http://www.electronics.dit.ie/staff/tscarff/Music_technology/midi/midi_note_numbers_for_octaves.htm for more information
-}
type alias Note = Int

{-| Common initial configuration for the component

Now it starts with no keys being pressed in a 25-key keyboard, in interactive
mode and with the size selector and the note debugger.
-}
initialModel : Model
initialModel =
    { notes = Set.empty
    , noteRange = keyboard25Keys
    , interactive = True
    , showSizeSelector = True
    , debugNotes = True
    }

{-| Note range of a 12-key keyboard
-}
keyboard12Keys : (Int, Int)
keyboard12Keys = (48, 59)

{-| Note range of a 25-key keyboard
-}
keyboard25Keys : (Int, Int)
keyboard25Keys = (36, 60)

{-| Note range of a 49-key keyboard
-}
keyboard49Keys : (Int, Int)
keyboard49Keys = (24, 72)

{-| Note range of a 61-key keyboard
-}
keyboard61Keys : (Int, Int)
keyboard61Keys = (24, 84)

{-| Note range of a 76-key keyboard
-}
keyboard76Keys : (Int, Int)
keyboard76Keys = (16, 91)

{-| Note range of a 88-key keyboard
-}
keyboard88Keys : (Int, Int)
keyboard88Keys = (9, 96)


-- UPDATE

{-| Messages received when clicking a key or
changing the keyboard's size
-}
type Msg
    = KeyUp Note
    | KeyDown Note
    | ChangeNoteRange (Int, Int)

{-| Handle the messages by updating model.notes or model.noteRange
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyUp note ->
            if model.interactive then
                { model | notes = Set.remove note model.notes }
            else
                model

        KeyDown note ->
            if model.interactive then
                { model | notes = Set.insert note model.notes }
            else
                model

        ChangeNoteRange size ->
            { model | noteRange = size }


-- VIEW

{-| Show the Piano component and, if set in the model, the debug text and the
keyboard size changer.
-}
view : Model -> Html Msg
view model =
    let
        container inner =
            div [class "piano"]
                [div [class "piano-container"]
                     [div [class "piano-keys"] inner]
                ]
        myStyle = node "style" [] [text css]

        range = [fst model.noteRange .. snd model.noteRange]

        sizeSelector =
            if model.showSizeSelector then
               let
                   keyboardOption size =
                       let
                           keys = snd size - fst size + 1
                       in
                           button [onClick (ChangeNoteRange size)]
                                  [text (toString keys ++ "-key piano")]
               in
                  List.map keyboardOption
                      [ keyboard12Keys
                      , keyboard25Keys
                      , keyboard49Keys
                      , keyboard61Keys
                      , keyboard76Keys
                      , keyboard88Keys]
                    |> List.intersperse (br [] [])
           else
               []

        debugNotes =
            if model.debugNotes then
                [ div [] [text <| "Currently pressed notes: " ++ String.join ", "
                    (model.notes |> Set.toList |> List.map noteName)
                    ] ]
            else
                []
    in
        span [style [("text-align", "center")]] ([myStyle
                , container <| List.map2 viewKey
                    range
                    (List.map (flip Set.member model.notes) range)
                ] ++ debugNotes ++ sizeSelector)

{-| Helper function to render a single note
-}
viewKey : Note -> Bool -> Html Msg
viewKey note active =
    if isNatural note then
        div
            [ classList [
                ("piano-white", True),
                ("pressed", active)
              ]
              , onMouseDown (KeyDown note)
              , onMouseUp (KeyUp note)
            ] 
            []
    else
        div 
            [class "piano-black"] 
            [ div [ 
                classList [
                    ("piano-black-raised", True),
                    ("pressed", active)
                ],
                onMouseDown (KeyDown note),
                onMouseUp (KeyUp note)
              ] [] ]


-- Note helpers

{-| Octave number of a note
-}
octave : Note -> Int
octave note =
    note // 12

{-| Return False is note is a flat or sharp, True otherwise
-}
isNatural : Note -> Bool
isNatural note =
    List.member (note % 12) [0, 2, 4, 5, 7, 9, 11]

{-| Represent a note number as a string
-}
noteName : Note -> String
noteName note =
    let
        getCharAt n str = String.slice n (n + 1) str
        noteName' = getCharAt (note % 12) "CCDDEFFGGAAB"
        alteration = if isNatural note then "" else "#"
    in
       noteName' ++ alteration ++ toString (octave note)
