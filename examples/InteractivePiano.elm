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
    , piano : Piano.Model
    }


init : ( Model, Cmd Msg )
init =
    Model False Piano.initialModel ! []



-- UPDATE


type Msg
    = MidiJSLoaded
    | PianoEvent Piano.Msg


port noteOn : Int -> Cmd msg


port noteOff : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MidiJSLoaded ->
            { model | midiJSLoaded = True } ! []

        PianoEvent pianoMsg ->
            let
                cmd =
                    case pianoMsg of
                        Piano.KeyUp note ->
                            noteOff note

                        Piano.KeyDown note ->
                            noteOn note

                        _ ->
                            Cmd.none
            in
                ( { model | piano = Piano.update pianoMsg model.piano }, cmd )



-- SUBSCRIPTIONS


port midiJSLoaded : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    midiJSLoaded (always MidiJSLoaded)



-- VIEW


view : Model -> Html Msg
view model =
    if model.midiJSLoaded then
        div []
            [ Html.map PianoEvent (Piano.view model.piano)
            ]
    else
        div [] [ text "MIDI.js not loaded" ]
