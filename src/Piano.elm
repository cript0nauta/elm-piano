module Piano
    exposing
        ( Msg
        , Note
        , Config
        , config
        , State
        , CurrentKeys
        , pressedKeys
        , newKeys
        , releasedKeys
        , initialState
        , setNotes
        , getNotes
        , updateNotes
        , interactive
        , update
        , view
        , colorAllPressedKeys
        , colorAllUnpressedKeys
        , colorPressedKeys
        , colorUnpressedKeys
        , isNatural
        , noteName
        , octave
        , allNotes
        , keyboard12Keys
        , keyboard25Keys
        , keyboard49Keys
        , keyboard61Keys
        , keyboard76Keys
        , keyboard88Keys
        )

{-| A customizable piano component


# Model

@docs colorAllUnpressedKeys
@docs colorAllPressedKeys
@docs Note
@docs allNotes


# Messages and updates

@docs Msg


# HTML rendering

@docs view


# Note helpers

@docs noteName
@docs isNatural
@docs octave


# Keyboard size helpers

@docs keyboard12Keys
@docs keyboard25Keys
@docs keyboard49Keys
@docs keyboard61Keys
@docs keyboard76Keys
@docs keyboard88Keys


# TODO doc

@docs Config
@docs initialState
@docs setNotes
@docs getNotes
@docs State
@docs config
@docs update
@docs interactive
@docs CurrentKeys
@docs pressedKeys
@docs newKeys
@docs releasedKeys
@docs updateNotes
@docs colorPressedKeys
@docs colorUnpressedKeys

-}

import Css exposing (..)
import Color
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Set exposing (Set)
import String


-- main =
--     App.beginnerProgram
--         { model = initialModel
--         , update = update
--         , view = view
--         }
-- MODEL


{-| The model of the component.

notes is the set of currently pressed notes.

noteRange determines the first and last notes shown in keyboard.

If interactive is True, the component will generate KeyUp and KeyDown messages
when the user clicks on a note. (Now this mode is experimental and has some
UI issues).

If showSizeSelector is True a button group will be shown to select the keyboard
size.

If debugNotes is True a text will appear, showing the note names of each
currently pressed note.

pressedKeyColors and unpressedKeyColors are dictionaries that override the
default color of the keys when they are, respectively, pressed or unpressed,
so they allow the user to specify custom colors for each keys

-}
type alias Model =
    { notes : Set Note
    , noteRange : ( Note, Note )
    , interactive : Bool
    , showSizeSelector : Bool
    , debugNotes : Bool
    , pressedKeyColors : Dict Note Color.Color
    , unpressedKeyColors : Dict Note Color.Color
    }


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


{-| Common initial configuration for the component

Now it starts with no keys being pressed in a 25-key keyboard, in interactive
mode and without the size selector nor the note debugger.

-}
initialModel : Model
initialModel =
    { notes = Set.empty
    , noteRange = keyboard25Keys
    , interactive = True
    , showSizeSelector = False
    , debugNotes = False
    , pressedKeyColors = Dict.empty
    , unpressedKeyColors = Dict.empty
    }


{-| TODO
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { noteRange : ( Note, Note )
    , pressedKeyColors : Dict Note Color.Color
    , unpressedKeyColors : Dict Note Color.Color
    , update : Maybe (Msg -> msg)
    }


{-| TODO
-}
config : ( Note, Note ) -> Config msg
config noteRange =
    Config <|
        ConfigInternal
            noteRange
            Dict.empty
            Dict.empty
            Nothing


{-| TODO
-}
interactive : (Msg -> yourMsg) -> Config a -> Config yourMsg
interactive f (Config config) =
    Config { config | update = Just f }


{-| TODO
-}
type State
    = State
        { notes : Set Note
        }


{-| TODO
-}
initialState : State
initialState =
    State { notes = Set.empty }


{-| TODO
-}
setNotes : Set Note -> State -> State
setNotes notes _ =
    State { notes = notes }


{-| TODO
-}
getNotes : State -> Set Note
getNotes (State { notes }) =
    notes


{-| TODO
-}
updateNotes : (Set Note -> Set Note) -> State -> State
updateNotes f (State { notes }) =
    State { notes = f notes }


colorKeys : Color.Color -> Color.Color -> Dict Note Color.Color
colorKeys white black =
    allNotes
        |> List.map
            (\n ->
                ( n
                , if isNatural n then
                    white
                  else
                    black
                )
            )
        |> Dict.fromList


{-| Does the same that colorAllUnpressedKeys, but sets the color of the
pressed keys instead
-}
colorAllPressedKeys : Color.Color -> Color.Color -> Config msg -> Config msg
colorAllPressedKeys white black (Config config) =
    Config { config | pressedKeyColors = colorKeys white black }


{-| Update a Piano model by setting the color of all the unpressed keys.

It takes as parameter the desired color of the white and the black keys, in
that order.

Notice that if more keys are pressed after calling the function, the new keys
will also have this color, so there is no need of calling this function on
every update.

-}
colorAllUnpressedKeys : Color.Color -> Color.Color -> Config msg -> Config msg
colorAllUnpressedKeys white black (Config config) =
    Config { config | unpressedKeyColors = colorKeys white black }


{-| TODO
-}
colorPressedKeys : Dict Note Color.Color -> Config msg -> Config msg
colorPressedKeys d (Config config) =
    Config { config | pressedKeyColors = d }


{-| TODO
-}
colorUnpressedKeys : Dict Note Color.Color -> Config msg -> Config msg
colorUnpressedKeys d (Config config) =
    Config { config | unpressedKeyColors = d }


{-| Note range of a 12-key keyboard
-}
keyboard12Keys : ( Int, Int )
keyboard12Keys =
    ( 48, 59 )


{-| Note range of a 25-key keyboard
-}
keyboard25Keys : ( Int, Int )
keyboard25Keys =
    ( 36, 60 )


{-| Note range of a 49-key keyboard
-}
keyboard49Keys : ( Int, Int )
keyboard49Keys =
    ( 24, 72 )


{-| Note range of a 61-key keyboard
-}
keyboard61Keys : ( Int, Int )
keyboard61Keys =
    ( 24, 84 )


{-| Note range of a 76-key keyboard
-}
keyboard76Keys : ( Int, Int )
keyboard76Keys =
    ( 16, 91 )


{-| Note range of a 88-key keyboard
-}
keyboard88Keys : ( Int, Int )
keyboard88Keys =
    ( 9, 96 )



-- UPDATE


{-| Messages received when clicking a key or
changing the keyboard's size
-}
type Msg
    = KeyUp Note
    | KeyDown Note


{-| TODO
-}
type
    CurrentKeys
    -- TODO: write integration tests for this data structure
    = CurrentKeys
        { old : Set Note
        , new : Set Note
        }


{-| TODO
-}
pressedKeys : CurrentKeys -> Set Note
pressedKeys (CurrentKeys { new }) =
    new


{-| TODO
-}
newKeys : CurrentKeys -> Set Note
newKeys (CurrentKeys { old, new }) =
    Set.diff new old


{-| TODO
-}
releasedKeys : CurrentKeys -> Set Note
releasedKeys (CurrentKeys { old, new }) =
    Set.diff old new


{-| Handle the messages by updating model.notes or model.noteRange.

You won't need to use this if you are using a non interactive
keyboard without the keyboard size selector

-}
update : Msg -> State -> ( State, CurrentKeys )
update msg (State oldState) =
    let
        toTuple : State -> ( State, CurrentKeys )
        toTuple (State newState) =
            ( State newState
            , CurrentKeys
                { old = (oldState.notes)
                , new = (newState.notes)
                }
            )
    in
        toTuple <|
            updateInternal msg (State oldState)


updateInternal : Msg -> State -> State
updateInternal msg (State { notes }) =
    case msg of
        KeyUp note ->
            State { notes = Set.remove note notes }

        KeyDown note ->
            State { notes = Set.insert note notes }



-- VIEW


{-| Show the Piano component and, if set in the model, the debug text and the
keyboard size changer.
-}
view : Config msg -> State -> Html.Html msg
view (Config config) (State { notes }) =
    let
        container inner =
            div
                [ css
                    [ padding (px 5)
                    , margin2 (px 0) auto
                    ]
                ]
                [ div
                    [ css
                        [ textAlign center
                        , borderRadius (px 5)
                        , margin (px 5)
                        , padding (px 5)
                        , whiteSpace noWrap
                        ]
                    ]
                    [ div
                        [ css
                            [ letterSpacing (px 0)
                            , fontSize (px 0)
                            , Css.property "word-spacing" "0"
                            ]
                        ]
                        inner
                    ]
                ]

        range =
            List.range (Tuple.first config.noteRange) (Tuple.second config.noteRange)

        -- Convert from a native Color to a elm-css Color
        nativeColorToCss : Color.Color -> Color
        nativeColorToCss c =
            let
                { red, green, blue, alpha } =
                    Color.toRgb c
            in
                rgba red green blue alpha
    in
        span [ style [ ( "text-align", "center" ) ] ]
            ([ container <|
                List.map
                    (\note ->
                        let
                            active =
                                (Set.member note notes)

                            colorDict =
                                (if active then
                                    config.pressedKeyColors
                                 else
                                    config.unpressedKeyColors
                                )
                        in
                            viewKey
                                config
                                note
                                (Dict.get note colorDict
                                    |> Maybe.map nativeColorToCss
                                )
                                active
                    )
                    range
             ]
            )
            |> toUnstyled


{-| Helper function to render a single note
-}
viewKey : ConfigInternal msg -> Note -> Maybe Color -> Bool -> Html msg
viewKey config note color active =
    let
        blackWhiteStyle : Style
        blackWhiteStyle =
            Css.batch
                [ display inlineBlock
                , position relative
                , verticalAlign top
                , Css.property "direction" "ltr"
                , margin zero
                , padding zero
                ]

        keysBoderStyle : Style
        -- It was .piano-white .piano-black-raised
        keysBoderStyle =
            Css.batch
                [ borderRadius (px 2)
                , borderColor (hex "222")
                , borderStyle solid
                , borderWidth4 (px 1) (px 1) (px 1) (px 1)
                ]

        defaultColor =
            if isNatural note then
                if active then
                    hex "#88FFAA"
                else
                    hex "#FFFFFF"
            else if active then
                hex "#55AA55"
            else
                hex "#000000"
    in
        if isNatural note then
            div
                (List.singleton
                    (css
                        [ blackWhiteStyle
                        , keysBoderStyle
                        , Css.width (px 24)
                        , Css.height (px 100)
                        , color
                            |> Maybe.withDefault defaultColor
                            |> backgroundColor
                        , zIndex (int 1)
                        ]
                    )
                    |> event config onMouseDown (KeyDown note)
                    |> event config
                        onMouseUp
                        (KeyUp note)
                )
                []
        else
            div
                [ css
                    [ blackWhiteStyle
                    , Css.width zero
                    , Css.height zero
                    , zIndex (int 2)
                    ]
                ]
                [ div
                    (List.singleton
                        (css
                            [ Css.width (px 16)
                            , Css.height (px 70)
                            , position relative
                            , left (px (-10))
                            , color
                                |> Maybe.withDefault defaultColor
                                |> backgroundColor
                            , keysBoderStyle
                            ]
                        )
                        |> event config onMouseDown (KeyDown note)
                        |> event config onMouseUp (KeyUp note)
                    )
                    []
                ]


event : ConfigInternal msg -> (msg -> Attribute msg) -> Msg -> List (Attribute msg) -> List (Attribute msg)
event { update } f internalMsg attrs =
    case update of
        Just wrapperMsg ->
            (f <| (wrapperMsg internalMsg)) :: attrs

        Nothing ->
            attrs



-- Note helpers


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
