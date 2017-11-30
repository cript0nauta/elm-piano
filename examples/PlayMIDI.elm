port module PlayMIDI exposing (..)

import Color
import Dict
import Html as Html exposing (..)
import Html exposing (program)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Set
import PlayerController
import Piano


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { midiUrl : String
    , midiJSLoaded : Bool
    , midiFileLoaded : Bool
    , midiError : Maybe String
    , playerInfo : PlayerController.Model
    , piano : Piano.Model
    , colored : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { midiUrl = ""
            , midiJSLoaded = False
            , midiFileLoaded = False
            , midiError = (Just "Not loaded")
            , colored = True
            , playerInfo = PlayerController.model
            , piano =
                -- I have to do this due to compiler restrictions
                -- See https://github.com/elm-lang/elm-compiler/issues/635
                -- for details
                let
                    p =
                        Piano.initialModel
                in
                    { p
                        | noteRange = Piano.keyboard88Keys
                        , interactive = False
                    }
                        |> setPianoColors True
            }
    in
        ( model, Cmd.none )


{-| Given a list of keys, color them to make them look like a color wheel.
Returns a dict compatible with the Piano input
-}
colorWheelKeys : Float -> Float -> List Piano.Note -> Dict.Dict Piano.Note Color.Color
colorWheelKeys saturation lightness l =
    let
        setColor : Int -> Int -> Piano.Note -> ( Piano.Note, Color.Color )
        setColor max i note =
            let
                hue =
                    (pi * 2 * toFloat i / toFloat max)

                color =
                    Color.hsl hue saturation lightness
            in
                ( note, color )
    in
        l
            |> List.indexedMap (setColor (List.length l))
            |> Dict.fromList


setPianoColors : Bool -> Piano.Model -> Piano.Model
setPianoColors colored piano =
    let
        pressedKeyColors =
            if colored then
                -- Color all pressed notes with higher saturation colors
                Piano.allNotes
                    -- |> List.filter Piano.isNatural
                    |> colorWheelKeys 1 0.8
            else
                .pressedKeyColors Piano.initialModel

        unpressedKeyColors =
            if colored then
                -- Color only white keys, leave the black ones black
                Piano.allNotes
                    |> List.filter Piano.isNatural
                    |> colorWheelKeys 0.3 0.5
                -- |> Dict.union
                --     -- Color black keys
                --     (Piano.allNotes
                --         |> List.filter (not << Piano.isNatural)
                --         |> colorWheelKeys 0.2 0.4
                --     )
            else
                .pressedKeyColors Piano.initialModel
    in
        { piano
            | pressedKeyColors = pressedKeyColors
            , unpressedKeyColors = unpressedKeyColors
        }



-- UPDATE


type Msg
    = ChangeUrl String
    | LoadMIDI
    | LoadOK
    | LoadFailed String
    | MidiJSLoaded
    | ChangePlayerStatus PlayerController.Msg
    | PianoEvent Piano.Msg
    | NoteOn Int
    | NoteOff Int
    | ToggleColored


port loadMIDI : String -> Cmd msg


port resume : () -> Cmd msg


port pause : () -> Cmd msg


port stop : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl url ->
            ( { model | midiUrl = url }, Cmd.none )

        LoadMIDI ->
            ( { model | midiFileLoaded = False, midiError = Nothing }, loadMIDI model.midiUrl )

        LoadOK ->
            -- ({model | midiFileLoaded=True}, Cmd.none)
            ( { model | midiFileLoaded = True }, resume () )

        LoadFailed msg ->
            ( { model | midiFileLoaded = False, midiError = Just msg }, Cmd.none )

        MidiJSLoaded ->
            -- ({model | midiJSLoaded=True}, Cmd.none)
            ( { model | midiJSLoaded = True }, loadMIDI "midis/cabeza.mid" )

        ChangePlayerStatus playerMsg ->
            let
                playerModel =
                    PlayerController.update playerMsg model.playerInfo

                cmd =
                    case playerModel.status of
                        PlayerController.Playing ->
                            resume ()

                        PlayerController.Paused ->
                            pause ()

                        PlayerController.Stopped ->
                            stop ()
            in
                ( { model | playerInfo = playerModel }, cmd )

        PianoEvent pianoMsg ->
            ( { model | piano = Piano.update pianoMsg model.piano }, Cmd.none )

        NoteOn note ->
            let
                piano =
                    model.piano

                newPiano =
                    { piano | notes = Set.insert note model.piano.notes }
            in
                ( { model | piano = newPiano }, Cmd.none )

        NoteOff note ->
            let
                piano =
                    model.piano

                newPiano =
                    { piano | notes = Set.remove note model.piano.notes }
            in
                ( { model | piano = newPiano }, Cmd.none )

        ToggleColored ->
            { model | colored = not model.colored
            , piano = setPianoColors (not model.colored) model.piano} ! []



-- SUBSCRIPTIONS


port midiFileLoaded : (() -> msg) -> Sub msg


port midiJSLoaded : (() -> msg) -> Sub msg


port midiLoadFailed : (String -> msg) -> Sub msg


port noteOn : (Int -> msg) -> Sub msg


port noteOff : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ midiFileLoaded (\_ -> LoadOK)
        , midiLoadFailed LoadFailed
        , midiJSLoaded (\_ -> MidiJSLoaded)
        , noteOn NoteOn
        , noteOff NoteOff
        ]



-- VIEW


view : Model -> Html Msg
view model =
    if model.midiJSLoaded then
        div []
            [ input [ onInput ChangeUrl ] []
            , button [ onClick LoadMIDI ] [ text "Load MIDI file" ]
            , button
                [ onClick ToggleColored ]
                [ text
                    ((if model.colored then
                        "Disable"
                      else
                        "Enable"
                     )
                        ++ " colored keyboard"
                    )
                ]
            , div []
                [ text
                    (if model.midiFileLoaded then
                        "Loaded"
                     else
                        "Not loaded"
                    )
                ]
            , div [] [ text ("Errors: " ++ (withDefault "No Errors" model.midiError)) ]
            , div []
                (if model.midiFileLoaded then
                    [ Html.map ChangePlayerStatus <| PlayerController.view model.playerInfo ]
                 else
                    []
                )
            , Html.map PianoEvent (Piano.view model.piano)
            ]
    else
        div [] [ text "MIDI.js not loaded" ]
