port module InteractivePiano exposing (..)

import Html exposing (..)
import Html exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Set
import PlayerController
import Piano
import Json.Decode
import Utils exposing (debugNotes, sizeSelector)


-- FIXME Delete when issue #686 of elm-lang/core is solved.


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { midiJSLoaded : Bool
    , pianoState : Piano.State
    , pianoSize : ( Piano.Note, Piano.Note )
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        False
        Piano.initialState
        Piano.keyboard25Keys
    , Cmd.none
    )



-- UPDATE


type Msg
    = MidiJSLoaded
    | PianoEvent Piano.Msg
    | ChangePianoSize ( Piano.Note, Piano.Note )


port noteOn : Int -> Cmd msg


port noteOff : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MidiJSLoaded ->
            { model | midiJSLoaded = True } ! []

        PianoEvent pianoMsg ->
            let
                ( pianoState, keys ) =
                    Piano.update pianoMsg model.pianoState

                noteOnCmds : List (Cmd msg)
                noteOnCmds =
                    Piano.newKeys keys
                        |> Set.toList
                        |> List.map noteOn

                noteOffCmds : List (Cmd msg)
                noteOffCmds =
                    Piano.releasedKeys keys
                        |> Set.toList
                        |> List.map noteOff
            in
                ( { model | pianoState = pianoState }
                , Cmd.batch (noteOnCmds ++ noteOffCmds)
                )

        ChangePianoSize size ->
            ( { model | pianoSize = size }
            , Cmd.none
            )



-- SUBSCRIPTIONS


port midiJSLoaded : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    midiJSLoaded (always MidiJSLoaded)



-- VIEW


view : Model -> Html Msg
view model =
    let
        pianoConfig =
            Piano.config model.pianoSize
                |> Piano.interactive PianoEvent
    in
        if model.midiJSLoaded then
            div []
                [ Piano.view pianoConfig model.pianoState
                , debugNotes model.pianoState
                , sizeSelector ChangePianoSize
                ]
        else
            div [] [ text "MIDI.js not loaded" ]
