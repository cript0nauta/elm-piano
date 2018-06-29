port module PlayMIDI exposing (..)

import Browser exposing (Document)
import Color
import Dict
import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Set
import PlayerController
import Piano


main : Program () Model Msg
main =
    Browser.document
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
    , pianoState : Piano.State
    , colored : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { midiUrl = ""
            , midiJSLoaded = False
            , midiFileLoaded = False
            , midiError = (Just "Not loaded")
            , colored = True
            , playerInfo = PlayerController.initialModel
            , pianoState = Piano.initialState
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


setPianoColors : Bool -> Piano.Config msg -> Piano.Config msg
setPianoColors colored config =
    let
        pressedKeysDict =
            -- Color all pressed notes with higher saturation colors
            Piano.allNotes
                -- |> List.filter Piano.isNatural
                |> colorWheelKeys 1 0.8

        unpressedKeysDict =
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
    in
        if colored then
            config
                |> Piano.colorPressedKeys pressedKeysDict
                |> Piano.colorUnpressedKeys unpressedKeysDict
        else
            config



-- UPDATE


type Msg
    = ChangeUrl String
    | LoadMIDI
    | LoadOK
    | LoadFailed String
    | MidiJSLoaded
    | ChangePlayerStatus PlayerController.Msg
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

        LoadFailed err ->
            ( { model | midiFileLoaded = False, midiError = Just err }, Cmd.none )

        MidiJSLoaded ->
            -- ({model | midiJSLoaded=True}, Cmd.none)
            ( { model | midiJSLoaded = True }
            , loadMIDI "midis/cabeza.mid"
            )

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

        NoteOn note ->
            let
                newPiano =
                    model.pianoState
                        |> Piano.updateNotes (Set.insert note)
            in
                ( { model | pianoState = newPiano }, Cmd.none )

        NoteOff note ->
            let
                newPiano =
                    model.pianoState
                        |> Piano.updateNotes (Set.remove note)
            in
                ( { model | pianoState = newPiano }, Cmd.none )

        ToggleColored ->
            ( { model
                | colored = not model.colored
              }
            , Cmd.none
            )



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


viewHtml : Model -> Html Msg
viewHtml model =
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
            , Piano.view
                (Piano.makeConfig Piano.keyboard88Keys
                    |> setPianoColors model.colored
                )
                model.pianoState
            ]
    else
        div [] [ text "MIDI.js not loaded" ]


view : Model -> Document Msg
view model =
    { title = "Play MIDI"
    , body = [viewHtml model] }
