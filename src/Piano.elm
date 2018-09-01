module Piano
    exposing
        ( Msg
        , Note
        , Config
        , makeConfig
        , State
        , CurrentNotes
        , KeyColor
        , activeNotes
        , newNotes
        , releasedNotes
        , initialState
        , setNotes
        , getNotes
        , updateNotes
        , update
        , viewStatic
        , viewInteractive
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

{-| A reusable piano view


# View

@docs view


# State

@docs State
@docs initialState
@docs Note
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

@docs noteName
@docs isNatural
@docs octave
@docs allNotes


# Keyboard size helpers

@docs keyboard12Keys
@docs keyboard25Keys
@docs keyboard49Keys
@docs keyboard61Keys
@docs keyboard76Keys
@docs keyboard88Keys

-}

import Browser.Dom
import Css exposing (..)
import Dict exposing (Dict)
import Json.Decode
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Set exposing (Set)
import String
import Task exposing (Task)
import Piano.TouchEvents as Touch exposing (Touch, TouchEvent)


-- main =
--     App.beginnerProgram
--         { model = initialModel
--         , update = update
--         , view = view
--         }


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


{-| Configuration for the view.

**Note:** This should *never* be held in your model, since it is related
to the `view` code.

-}
type Config
    = Config ConfigInternal


type alias KeyColor =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


type alias ConfigInternal =
    { noteRange : ( Note, Note )
    , pressedKeyColors : Dict Note KeyColor
    , unpressedKeyColors : Dict Note KeyColor
    }


{-| Construct a basic configuration

It takes as its only argument a tuple holding the first and last notes that
should be displayed. You can use one of the values described in
[Keyboard size helpers](#keyboard-size-helpers) or specify your own.

    pianoConfig : Piano.Config
    pianoConfig =
        Piano.makeConfig Piano.keyboard88Keys

-}
makeConfig : ( Note, Note ) -> Config
makeConfig noteRange =
    Config <|
        ConfigInternal
            noteRange
            Dict.empty
            Dict.empty


{-| The view's internal state. You should keep this on your model
-}
type State
    = State StateInternal

-- Store the positions of each key. The first and last keys positions are
-- duplicated in a type-safe manner and used to decide when to refresh
-- all the keys because something changed in the document
type Coordinates
    = NothingFetched
    | FetchedBounds ( (Float, Float), (Float, Float) )
    | FetchedAll ( (Float, Float), (Float, Float) ) KeyCoordinates


type alias StateInternal =
    { touches : Dict String Note
    , mouse : MouseStatus
    , keyCoordinates : Coordinates
    }

type alias KeyCoordinates =
    List
        ( Note
        , { x : Float
          , y : Float
          , width : Float
          , height : Float
          }
        )


type MouseStatus
    = NotClicked
    | ClickedOutsideKeys
    | ClickedKey Note


{-| Contructor for the State type
-}
initialState : State
initialState =
    State
        { touches = Dict.empty
        , keyCoordinates = NothingFetched
        , mouse = NotClicked }


notes : StateInternal -> Set Note
notes state =
    let
        mouseNotes =
            case state.mouse of
                ClickedKey note ->
                    [ note ]
                _ ->
                    []
    in
    Set.fromList <|
        mouseNotes ++ Dict.values state.touches


{-| Set the currently pressed notes of the piano

    newPianoState =
        -- The only pressed key will be the middle C
        Piano.initialState
            |> Piano.setNotes (Set.singleton 48)

-}
setNotes : Set Note -> State -> State
setNotes desiredNotes (State state) =
    Debug.todo "not implemented"


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
getNotes (State state) =
    notes state


{-| Changes the currently pressed notes of a State

    newPianoState =
        -- If the middle C was pressed, unpress it
        -- the remaining keys won't change their pressed status
        oldPianoState
            |> Piano.updateNotes (Set.remove 48)

-}
updateNotes : (Set Note -> Set Note) -> State -> State
updateNotes f (State state) =
    Debug.todo "not implemented"


colorKeys : KeyColor -> KeyColor -> Dict Note KeyColor
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
colorAllPressedKeys : KeyColor -> KeyColor -> Config -> Config
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
colorAllUnpressedKeys : KeyColor -> KeyColor -> Config -> Config
colorAllUnpressedKeys white black (Config config) =
    Config { config | unpressedKeyColors = colorKeys white black }


{-| Same as `colorUnpressedKeys` but changes the color of pressed keys instead.
-}
colorPressedKeys : Dict Note KeyColor -> Config -> Config
colorPressedKeys d (Config config) =
    Config { config | pressedKeyColors = d }


{-| Override the default colors of the unpressed keys

Use it when the two functions above don't satisfy your needs (you probably
want to use different colors for each note)

-}
colorUnpressedKeys : Dict Note KeyColor -> Config -> Config
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
    | TouchEnd TouchEvent
    | TouchMove TouchEvent
    | SetKeyCoordinates KeyCoordinates
    | SetBoundsPosition ( (Float, Float), (Float, Float) )


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
update : Msg -> State -> ( State, CurrentNotes, Cmd Msg )
update msg (State oldState) =
    let
        ( newState, cmd ) =
            updateInternal msg oldState
    in
        ( State newState
        , CurrentNotes
            { old = (notes oldState)
            , new = (notes newState)
            }
        , cmd
        )


keyId : Note -> String
keyId note =
    "elm-piano-key-" ++ String.fromInt note


getKeyCoordinates : (Int, Int) -> Task Never KeyCoordinates
getKeyCoordinates (minNote, maxNote) =
    List.range minNote maxNote
    |> List.map
        (\note ->
            Browser.Dom.getElement (keyId note)
            |> Task.map
                (\res ->
                    ( note, res.element )
                )
            |> Task.map Just
            |> Task.onError (always (Task.succeed Nothing))
        )
    |> Task.sequence
    |> Task.map (List.filterMap identity)


getBoundsPosition : Task Never ( (Float, Float), (Float, Float) )
getBoundsPosition =
    let
        getPositionOf elementId =
            Browser.Dom.getElement elementId
            |> Task.map
                ( \res ->
                    ( res.element.x, res.element.y )
                )
            |> Task.onError (always (Task.succeed ( -1, -1 )))
    in
        Task.map2
            Tuple.pair
            (getPositionOf "elm-piano-first-key")
            (getPositionOf "elm-piano-last-key")


noCmd : StateInternal -> (StateInternal, Cmd Msg)
noCmd state =
    ( state, Cmd.none )


updateInternal : Msg -> StateInternal -> (StateInternal, Cmd Msg)
updateInternal msg state =
    case msg of
        Enter note ->
            ( case state.mouse of
                NotClicked ->
                    state

                ClickedOutsideKeys ->
                    { state | mouse = ClickedKey note }

                ClickedKey oldNote ->
                    { state | mouse = ClickedKey note }
            )
            |> noCmd

        Leave leaveNote ->
            ( case state.mouse of
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
                        { state | mouse = ClickedOutsideKeys }
            )
            |> noCmd

        Click note ->
            ( case state.mouse of
                NotClicked ->
                    { state | mouse = ClickedKey note }

                _ ->
                    state
                        |> Debug.log "Piano: detected click event with the mouse already clicked. Check this"
            )
            |> noCmd

        MouseUp ->
            ( { state | mouse = NotClicked }
            , Cmd.none
            )

        LeaveContainer ->
            -- MouseUp events that happen outside the container won't be
            -- registered, so it's better to force the status to be not clicked
            ( { state | mouse = NotClicked }
            , Cmd.none
            )

        TouchStart note {touches} ->
            let
                newTouches =
                    List.map
                        (\touch ->
                            ( touch.identifier, note )
                        )
                        touches
                        |> Dict.fromList
            in
            ( { state | touches = Dict.union newTouches state.touches }
            , Task.perform SetBoundsPosition getBoundsPosition
            )

        TouchEnd {touches} ->
            ( { state | touches =
                List.foldl
                    (.identifier >> Dict.remove)
                    state.touches
                    touches
              }
            , Cmd.none
            )

        TouchMove {touches} ->
            case state.keyCoordinates of
                FetchedAll _ coordinates ->
                    let
                        newTouches =
                            List.map (processTouchEvent coordinates) touches
                            |> List.filterMap identity
                            |> List.foldl
                                (\( identifier, note ) dict ->
                                    Dict.insert identifier note dict
                                )
                                state.touches
                    in
                        ( { state | touches = newTouches }
                        , Cmd.none
                        )
                _ ->
                    -- Ignore because we don't have the key coordinates that
                    -- are needed to decide which key is being pressed
                    ( state, Cmd.none )

        SetKeyCoordinates coords ->
            ( case state.keyCoordinates of
                NothingFetched ->
                    Debug.todo "This shouldn't happen!"

                FetchedBounds bounds ->
                    { state | keyCoordinates = FetchedAll bounds coords }

                FetchedAll bounds _ ->
                    { state | keyCoordinates = FetchedAll bounds coords }
            , Cmd.none
            )

        SetBoundsPosition coords ->
            case state.keyCoordinates of
                NothingFetched ->
                    ( { state | keyCoordinates = FetchedBounds coords }
                    , Task.perform
                        SetKeyCoordinates
                        (getKeyCoordinates keyboard88Keys)
                    )

                FetchedBounds oldCoords ->
                    if oldCoords == coords then
                        ( state, Cmd.none )
                    else
                        ( { state | keyCoordinates = FetchedBounds coords }
                        , Task.perform
                            SetKeyCoordinates
                            (getKeyCoordinates keyboard88Keys)
                        )

                FetchedAll oldCoords _ ->
                    if oldCoords == coords then
                        ( state, Cmd.none )
                    else
                        ( { state | keyCoordinates = FetchedBounds coords }
                        , Task.perform
                            SetKeyCoordinates
                            (getKeyCoordinates keyboard88Keys)
                        )


processTouchEvent : KeyCoordinates -> Touch -> Maybe ( String, Note )
processTouchEvent keyCoordinates touch =
    let
        (touchX, touchY) =
            touch.coordinates

        matchesCoordinates (_, {x, y, width, height}) =
            touchX >= x &&
            touchX <= x + width &&
            touchY >= y &&
            touchY <= y + height
    in
    find matchesCoordinates keyCoordinates
    |> Maybe.map (\(note, _) -> ( touch.identifier, note ))


find : (a -> Bool) -> List a -> Maybe a
find predicate l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x
            else
                find predicate xs


-- VIEW


type MessageType msg
    = NoMessages
    | WithMessages (Msg -> msg)


withMessages : MessageType Msg
withMessages =
    WithMessages identity


attrs : MessageType msg -> List (Attribute Never) -> List (Attribute Msg) -> List (Attribute msg)
attrs mt static stateful =
    let
        mappedStatic =
            List.map
                (Html.Styled.Attributes.map never)
                static
    in
        case mt of
            NoMessages ->
                mappedStatic

            WithMessages f ->
                List.append
                    mappedStatic
                    (List.map
                        (Html.Styled.Attributes.map f)
                        stateful)


{-| Show an interactive piano given its configuration and its state.

**Note**: The piano `State` should live in your model, but the `Config` shouldn't,
since it belongs to your `view` code. For more information about why you should
do this read
[this](https://github.com/evancz/elm-sortable-table/#about-api-design) explaination
(it was done for the great `elm-sortable-table` library whose API inspired me).

-}
viewInteractive : Config -> State -> Html.Html Msg
viewInteractive (Config config) (State state) =
    view withMessages config (notes state)

viewStatic : Config -> Set Note -> Html.Html Never
viewStatic (Config config) desiredNotes =
    view
        NoMessages
        config
        desiredNotes

view : MessageType msg -> ConfigInternal -> Set Note -> Html.Html msg
view mt config pressedNotes =
    let
        container inner =
            div
                (attrs mt
                    [ css
                        [ padding (px 5)
                        , margin2 (px 0) auto
                        ]
                    ]
                    [ onMouseUp MouseUp
                    , onMouseLeave LeaveContainer
                    ]
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
    in
        span [ style "text-align" "center" ]
            ([ container <|
                List.map
                    (\note ->
                        let
                            active =
                                (Set.member note pressedNotes)

                            colorDict =
                                (if active then
                                    config.pressedKeyColors
                                 else
                                    config.unpressedKeyColors
                                )

                            keyOrder =
                                if note == Tuple.first config.noteRange then
                                    First
                                else if note == Tuple.second config.noteRange then
                                    Last
                                else
                                    Middle
                        in
                            viewKey
                                mt
                                note
                                { active = active
                                , color = Dict.get note colorDict
                                , order = keyOrder
                                }
                    )
                    range
             ]
            )
            |> toUnstyled


type KeyOrder
    = First
    | Middle
    | Last


type alias KeyProperties =
    { active : Bool
    , color : Maybe KeyColor
    , order : KeyOrder
    }


{-| Helper function to render a single note
-}
viewKey : MessageType msg -> Note -> KeyProperties -> Html msg
viewKey mt note { order, color, active } =
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
            preventDefaultOn
                "mousedown"
                (Json.Decode.succeed (msg, True))

        keyInteractiveAttrs =
            [ onMouseEnter (Enter note)
            , onMouseLeave (Leave note)
            , onMouseDown_ (Click note)
            , Touch.onTouchStart (TouchStart note)
            , Touch.onTouchEnd TouchEnd
            , Touch.onTouchMove TouchMove
            ]

        -- used to fetch the positions of the first and last keys
        keyInner =
            case order of
                First ->
                    [ span
                        [ id "elm-piano-first-key" ]
                        []
                    ]

                Last ->
                    [ span
                        [ id "elm-piano-last-key" ]
                        []
                    ]

                Middle ->
                    []
    in
        if isNatural note then
            div
                (attrs mt
                    [ css
                        [ blackWhiteStyle
                        , keysBoderStyle
                        , Css.width (px 24)
                        , Css.height (px 100)
                        , backgroundColor (Maybe.map toCssColor color
                            |> Maybe.withDefault defaultColor)
                        , zIndex (int 1)
                        ]
                    , id (keyId note)
                    ]
                    keyInteractiveAttrs
                )
                keyInner
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
                    (attrs mt
                        [ css
                            [ Css.width (px 16)
                            , Css.height (px 70)
                            , position relative
                            , left (px (-10))
                            , backgroundColor (Maybe.map toCssColor color
                                |> Maybe.withDefault defaultColor)
                            , keysBoderStyle
                            ]
                        , id (keyId note)
                        ]
                        keyInteractiveAttrs
                    )
                    keyInner
                ]


toCssColor : KeyColor -> Color
toCssColor color =
    rgba color.red color.green color.blue color.alpha


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
    List.member (modBy 12 note) [ 0, 2, 4, 5, 7, 9, 11 ]


{-| Represent a note number as a string
-}
noteName : Note -> String
noteName note =
    let
        getCharAt n str =
            String.slice n (n + 1) str

        noteName_ =
            getCharAt (modBy 12 note) "CCDDEFFGGAAB"

        alteration =
            if isNatural note then
                ""
            else
                "#"
    in
        noteName_ ++ alteration ++ String.fromInt (octave note)
