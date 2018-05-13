module PlayerController exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- main =
--     beginnerProgram
--         { model = model
--         , view = view
--         , update = update
--         }



-- MODEL


type alias Model =
    { status : PlayerStatus
    }


initialModel : Model
initialModel =
    Model Stopped


type PlayerStatus
    = Playing
    | Paused
    | Stopped



-- UPDATE


type Msg
    = PlayPause
    | Resume
    | Pause
    | Stop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resume ->
            { model | status = Playing }

        Pause ->
            { model | status = Paused }

        Stop ->
            { model | status = Stopped }

        PlayPause ->
            let
                newStatus =
                    case model.status of
                        Playing ->
                            Paused

                        Paused ->
                            Playing

                        other ->
                            other
            in
                { model | status = newStatus }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Stop ] [ text "Stop" ]
        , playPause model.status
        ]


playPause : PlayerStatus -> Html Msg
playPause status =
    case status of
        Playing ->
            button [ onClick Pause ] [ text "Pause" ]

        _ ->
            button [ onClick Resume ] [ text "Play" ]
