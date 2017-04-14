port module PlayMIDI exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Set
import PlayerController
import Piano
import Json.Decode


-- FIXME Delete when issue #686 of elm-lang/core is solved.


main =
    App.program
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
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { midiUrl = ""
            , midiJSLoaded = False
            , midiFileLoaded = False
            , midiError = (Just "Not loaded")
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
                        | noteRange = Piano.keyboard61Keys
                        , interactive = False
                    }
            }
    in
        ( model, Cmd.none )



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
            , button [ onClick LoadMIDI ] [ text "Cargar" ]
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
                    [ App.map ChangePlayerStatus <| PlayerController.view model.playerInfo ]
                 else
                    []
                )
            , App.map PianoEvent (Piano.view model.piano)
            ]
    else
        div [] [ text "MIDI.js not loaded" ]
