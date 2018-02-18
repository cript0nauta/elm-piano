module Piano
    exposing
        ( Msg
          -- , Note
        , Config
        , config
        , State
        , CurrentNotes
        , activeNotes
        , newNotes
        , releasedNotes
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
          -- , isNatural
          -- , noteName
          -- , octave
          -- , allNotes
        , keyboard12Keys
        , keyboard25Keys
        , keyboard49Keys
        , keyboard61Keys
        , keyboard76Keys
        , keyboard88Keys
        )

{-| A reusable piano view


# View

@docs view


# State

@docs State
@docs initialState
docs Note
@docs getNotes
@docs setNotes
@docs updateNotes


# Configuration

@docs Config
@docs config
@docs interactive


# Interactive mode functions and types

Ignore this section if only want to display some notes rather than letting
the user to select the notes by clicking on the piano keys.

@docs Msg
@docs update
@docs CurrentNotes
@docs activeNotes
@docs newNotes
@docs releasedNotes


# Customizations

@docs colorAllUnpressedKeys
@docs colorAllPressedKeys
@docs colorUnpressedKeys
@docs colorPressedKeys


# Note helpers

docs noteName
docs isNatural
docs octave
docs allNotes


# Keyboard size helpers

@docs keyboard12Keys
@docs keyboard25Keys
@docs keyboard49Keys
@docs keyboard61Keys
@docs keyboard76Keys
@docs keyboard88Keys

-}

import Css exposing (..)
import Color
import Dict exposing (Dict)
import Json.Decode
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Set exposing (Set)
import Piano.Note exposing (..)
import Piano.TouchEvents exposing (..)


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


{-| Configuration for the view.

**Note:** This should *never* be held in your model, since it is related
to the `view` code.

-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { noteRange : ( Note, Note )
    , pressedKeyColors : Dict Note Color.Color
    , unpressedKeyColors : Dict Note Color.Color
    , update : Maybe (Msg -> msg)
    }


{-| Construct a basic configuration

It takes as its only argument a tuple holding the first and last notes that
should be displayed. You can use one of the values described in
[Keyboard size helpers](#keyboard-size-helpers) or specify your own.

    pianoConfig : Piano.Config msg
    pianoConfig =
        Piano.config Piano.keyboard88Keys

-}
config : ( Note, Note ) -> Config msg
config noteRange =
    Config <|
        ConfigInternal
            noteRange
            Dict.empty
            Dict.empty
            Nothing


{-| Sets up interactive mode for the view

    type Msg
        = PianoEvent Piano.Msg
        | OtherEvent

    pianoConfig =
        Piano.config Piano.keyboard88Keys
            |> Piano.interactive PianoEvent

Pass it a constructor that wraps an internal message into your own message
type. In other libraries this would be what you pass to the `Html.map` function

-}
interactive : (Msg -> yourMsg) -> Config a -> Config yourMsg
interactive f (Config config) =
    Config { config | update = Just f }


{-| The view's internal state. You should keep this on your model
-}
type State
    = State StateInternal


type alias StateInternal =
    { presses : Dict String PressStatus
    }


type MouseStatus
    = NotClicked
    | ClickedOutsideKeys
    | ClickedKey Note


{-| Like a maybe but more verbose
-}
type PressStatus
    = NothingPressed
    | KeyPressed Note


pressStatusToMaybe : PressStatus -> Maybe Note
pressStatusToMaybe status =
    case status of
        NothingPressed ->
            Nothing

        KeyPressed note ->
            Just note


clickPressKey : String
clickPressKey =
    -- A string that doesn't collapse with Touch identifiers
    "_PIANO_MOUSE_CLICK"


{-| Contructor for the State type
-}
initialState : State
initialState =
    State { presses = Dict.empty }


{-| Set the currently pressed notes of the piano

    newPianoState =
        -- The only pressed key will be the middle C
        Piano.initialState
            |> Piano.setNotes (Set.singleton 48)

-}
setNotes : Set Note -> State -> State
setNotes notes _ =
    -- State { state | notes = notes }
    Debug.crash ""


{-| Returns the currently pressed notes of a State

    showPressedNotes : Piano.State -> Html msg
    showPressedNotes state =
        text <|
            "Currently pressed notes: "
                ++ String.join ", "
                    (Piano.getNotes state
                        |> Set.toList
                        |> List.map Piano.noteName
                    )

-}
getNotes : State -> Set Note
getNotes (State { presses }) =
    Dict.values presses
        |> List.filterMap pressStatusToMaybe
        |> Set.fromList


{-| Changes the currently pressed notes of a State

    newPianoState =
        -- If the middle C was pressed, unpress it
        -- the remaining keys won't change their pressed status
        oldPianoState
            |> Piano.updateNotes (Set.remove 48)

-}
updateNotes : (Set Note -> Set Note) -> State -> State
updateNotes f _ =
    -- State { state | notes = f state.notes }
    Debug.crash ""


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


{-| Update a Piano configuaration by setting the color of all the unpressed
keys.

It takes as parameter the desired color of the white and the black keys, in
that order.

    pianoConfig =
        Piano.config Piano.keyboard88Keys
            |> Piano.colorAllUnpressedKeys Color.lightOrange Color.darkOrange

-}
colorAllUnpressedKeys : Color.Color -> Color.Color -> Config msg -> Config msg
colorAllUnpressedKeys white black (Config config) =
    Config { config | unpressedKeyColors = colorKeys white black }


{-| Same as `colorUnpressedKeys` but changes the color of pressed keys instead.
-}
colorPressedKeys : Dict Note Color.Color -> Config msg -> Config msg
colorPressedKeys d (Config config) =
    Config { config | pressedKeyColors = d }


{-| Override the default colors of the unpressed keys

Use it when the two functions above don't satisfy your needs (you probably
want to use different colors for each note)

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


{-| An opaque type representing messages that are passed inside the Piano view.
-}
type Msg
    = Enter Note
    | Leave Note
    | Click Note
    | MouseUp
    | LeaveContainer
    | TouchStart Note TouchEvent
    | TouchMove ( Note, Note ) TouchEvent
    | TouchEnd TouchEvent


{-| A data structure used for child-parent communication in the `update` function

This follows the OutMsg pattern explained [here](https://medium.com/@_rchaves_/child-parent-communication-in-elm-outmsg-vs-translator-vs-nomap-patterns-f51b2a25ecb1)

-}
type
    CurrentNotes
    -- TODO: write integration tests for this data structure
    = CurrentNotes
        { old : Set Note
        , new : Set Note
        }


{-| Return the currently pressed notes of a CurrentNotes object.
-}
activeNotes : CurrentNotes -> Set Note
activeNotes (CurrentNotes { new }) =
    new


{-| Return a set of the notes that were pressed in the last event and
were unpressed before

    ( pianoState, currentNotes ) =
        Piano.update pianoMsg model.pianoState

    noteOnCmds : List (Cmd msg)
    noteOnCmds =
        Piano.newNotes currentNotes
            |> Set.toList
            |> List.map noteOn

-}
newNotes : CurrentNotes -> Set Note
newNotes (CurrentNotes { old, new }) =
    Set.diff new old


{-| Return a set of the notes that were unpressed in the last event and
were pressed before

    ( pianoState, currentNotes ) =
        Piano.update pianoMsg model.pianoState

    noteOffCmds : List (Cmd msg)
    noteOffCmds =
        Piano.releasedNotes currentNotes
            |> Set.toList
            |> List.map noteOff

-}
releasedNotes : CurrentNotes -> Set Note
releasedNotes (CurrentNotes { old, new }) =
    Set.diff old new


{-| Use this function to update the piano State.

The second tuple member is intented for you to use it to do some actions
in your code, like sending messages to a port producing sound
(see the interactive example).

    type Msg =
        PianoEvent Piano.Msg
        | OtherEvent

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            PianoEvent pianoMsg =
                let
                    -- I don't need to use the CurrentNotes right now
                    (newPianoState, _) =
                        Piano.update pianoMsg model.piano
                in
                    { model | piano = newPianoState }

-}
update : Msg -> State -> ( State, CurrentNotes )
update msg (State oldState) =
    let
        toTuple : StateInternal -> ( State, CurrentNotes )
        toTuple newState =
            ( State newState
            , CurrentNotes
                { old = getNotes (State oldState)
                , new = getNotes (State newState)
                }
            )
    in
        toTuple <|
            updateInternal msg (oldState)


updateInternal : Msg -> StateInternal -> StateInternal
updateInternal msg ({ presses } as state) =
    let
        mouse =
            pressesToMouseStatus presses
    in
        case msg of
            Enter note ->
                case mouse of
                    NotClicked ->
                        state

                    ClickedOutsideKeys ->
                        { state
                            | presses =
                                presses
                                    |> Dict.insert clickPressKey (KeyPressed note)
                        }

                    ClickedKey oldNote ->
                        { state
                            | presses =
                                presses
                                    |> Dict.insert clickPressKey (KeyPressed note)
                                    |> Debug.log "Piano: entering a key without prior leave"
                        }

            Leave leaveNote ->
                case mouse of
                    NotClicked ->
                        state

                    ClickedOutsideKeys ->
                        state
                            |> Debug.log "Piano: detected note leave with mouse outside piano keys"

                    ClickedKey clickNote ->
                        let
                            debug =
                                if clickNote == leaveNote then
                                    state
                                else
                                    state
                                        |> Debug.log "Piano: detected note leave with different pressed note"
                        in
                            { state | presses = Dict.insert clickPressKey NothingPressed presses }

            Click note ->
                case mouse of
                    NotClicked ->
                        { state | presses = Dict.insert clickPressKey (KeyPressed note) presses }

                    _ ->
                        state
                            |> Debug.log "Piano: detected click event with the mouse already clicked. Check this"

            MouseUp ->
                { state | presses = Dict.remove clickPressKey presses }

            LeaveContainer ->
                -- MouseUp events that happen outside the container won't be
                -- registered, so it's better to force the status to be not clicked
                { state | presses = Dict.remove clickPressKey presses }

            TouchStart note evt ->
                let
                    foldPresses : Touch -> Dict String PressStatus -> Dict String PressStatus
                    foldPresses t p =
                        case (fromCoordinates evt t.coordinates) of
                            Nothing ->
                                Dict.insert t.identifier (NothingPressed) p
                                    |> Debug.log "no key pressed"

                            Just note ->
                                Dict.insert t.identifier (KeyPressed note) p

                    newPresses =
                        List.foldr
                            foldPresses
                            presses
                            evt.touches
                in
                    { state
                        | presses = newPresses
                    }

            TouchEnd evt ->
                { state
                    | presses =
                        List.foldr
                            (\t p -> Dict.remove t.identifier p)
                            presses
                            evt.touches
                }

            TouchMove ( firstNote, lastNote ) evt ->
                let
                    maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
                    maybeFilter f =
                        Maybe.andThen
                            (\a ->
                                if f a then
                                    Just a
                                else
                                    Nothing
                            )

                    isVisibleNote : Note -> Bool
                    isVisibleNote note =
                        -- Return False if the note is out of visible boundaries
                        firstNote <= note && note <= lastNote

                    foldPresses touch p =
                        let
                            status =
                                (fromCoordinates
                                    evt
                                    touch.coordinates
                                )
                                    |> maybeFilter isVisibleNote
                                    |> Maybe.map KeyPressed
                                    |> Maybe.withDefault NothingPressed
                        in
                            Dict.insert touch.identifier status p
                in
                    { state
                        | presses =
                            List.foldr
                                foldPresses
                                presses
                                evt.touches
                    }


pressesToMouseStatus : Dict String PressStatus -> MouseStatus
pressesToMouseStatus d =
    case (Dict.get clickPressKey d) of
        Nothing ->
            NotClicked

        Just NothingPressed ->
            ClickedOutsideKeys

        Just (KeyPressed note) ->
            ClickedKey note



-- VIEW


{-| Show the piano given its configuration and its state.

**Note**: The piano `State` should live in your model, but the `Config` shouldn't,
since it belongs to your `view` code. For more information about why you should
do this read
[this](https://github.com/evancz/elm-sortable-table/#about-api-design) explaination
(it was done for the great `elm-sortable-table` library whose API inspired me).

-}
view : Config msg -> State -> Html.Html msg
view (Config config) state =
    let
        notes =
            getNotes state

        container inner =
            div
                (List.singleton
                    (css
                        [ padding (px 5)
                        , margin2 (px 0) auto
                        ]
                    )
                    |> event config onMouseUp MouseUp
                    |> event config onMouseLeave LeaveContainer
                )
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

        onMouseDown_ msg =
            -- Use preventDefault to prevent native drag & drop feature
            onWithOptions
                "mousedown"
                { defaultOptions | preventDefault = True }
                (Json.Decode.succeed msg)
    in
        if isNatural note then
            div
                ([ css
                    [ blackWhiteStyle
                    , keysBoderStyle
                    , Css.width (px 24)
                    , Css.height (px 100)
                    , color
                        |> Maybe.withDefault defaultColor
                        |> backgroundColor
                    , zIndex (int 1)
                    ]
                 , attribute "data-note" (toString note)
                 ]
                    |> event config onMouseEnter (Enter note)
                    |> event config onMouseLeave (Leave note)
                    |> event config onMouseDown_ (Click note)
                    |> touchEvents config note
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
                    ([ css
                        [ Css.width (px 16)
                        , Css.height (px 70)
                        , position relative
                        , left (px (-10))
                        , color
                            |> Maybe.withDefault defaultColor
                            |> backgroundColor
                        , keysBoderStyle
                        ]
                     , attribute "data-note" (toString note)
                     ]
                        |> event config onMouseEnter (Enter note)
                        |> event config onMouseLeave (Leave note)
                        |> event config onMouseDown_ (Click note)
                        |> touchEvents config note
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


touchEvents : ConfigInternal msg -> Note -> List (Attribute msg) -> List (Attribute msg)
touchEvents { update, noteRange } note l =
    case update of
        Nothing ->
            l

        Just w ->
            onTouchStart (w << TouchStart note)
                :: onTouchMove (w << TouchMove noteRange)
                :: onTouchEnd (w << TouchEnd)
                :: l
