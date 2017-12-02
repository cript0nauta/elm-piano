module Basic exposing (..)

import Html exposing (..)
import Color
import Set
import Piano


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { piano : Piano.Model
    }


init : Model
init =
    let
        dMajorKeys =
            [ 2, 6, 9 ]

        -- D, F#, A
        -- Show the notes D, F#, A in all the octaves
        shouldDisplayNote : Piano.Note -> Bool
        shouldDisplayNote =
            flip rem 12 >> flip List.member dMajorKeys

        notes =
            List.range 0 127
                |> List.filter shouldDisplayNote
                |> Set.fromList
    in
        { piano =
            -- I have to do this due to compiler restrictions
            -- See https://github.com/elm-lang/elm-compiler/issues/635
            -- for details
            let
                p =
                    Piano.initialModel
                        |> Piano.colorAllPressedKeys
                            Color.lightOrange
                            Color.darkOrange
            in
                { p
                    | noteRange = Piano.keyboard61Keys
                    , notes = notes
                    , interactive = False
                    , debugNotes = True
                }
        }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map (always NoOp) (Piano.view model.piano)
        ]
