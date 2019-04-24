port module InteractivePiano exposing (Model, Msg(..), init, main, midiJSLoaded, noteOff, noteOn, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Piano
import PlayerController
import Set
import Utils exposing (debugNotes, sizeSelector)


main =
    Browser.document
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { midiJSLoaded = False
      , pianoState = Piano.initialState
      , pianoSize = Piano.keyboard25Keys
      }
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
            ( { model | midiJSLoaded = True }
            , Cmd.map PianoEvent Piano.getKeyPositions
            )

        PianoEvent pianoMsg ->
            let
                ( pianoState, keys, pianoCmd ) =
                    Piano.update pianoMsg model.pianoState

                noteOnCmds : List (Cmd msg)
                noteOnCmds =
                    Piano.newNotes keys
                        |> Set.toList
                        |> List.map noteOn

                noteOffCmds : List (Cmd msg)
                noteOffCmds =
                    Piano.releasedNotes keys
                        |> Set.toList
                        |> List.map noteOff
            in
            ( { model | pianoState = pianoState }
            , Cmd.batch
                (Cmd.map PianoEvent pianoCmd
                    :: noteOnCmds
                    ++ noteOffCmds
                )
            )

        ChangePianoSize size ->
            ( { model | pianoSize = size }
            , Cmd.map PianoEvent Piano.getKeyPositions
            )



-- SUBSCRIPTIONS


port midiJSLoaded : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    midiJSLoaded (always MidiJSLoaded)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    if model.midiJSLoaded then
        { title = "Interactive Piano"
        , body =
            [ Piano.viewInteractive
                (Piano.makeConfig model.pianoSize)
                model.pianoState
                |> Html.map PianoEvent
            , if Piano.isTouchReady model.pianoState then
                debugNotes model.pianoState

              else
                div
                    [ style "text-align" "center" ]
                    [ text "Preparing multi-touch support..." ]
            , sizeSelector ChangePianoSize
            ]
        }

    else
        { title = "Loading..."
        , body = [ text "Waiting for MIDI.js to load..." ]
        }
