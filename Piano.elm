module Piano exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set
import String
import PianoStyle


main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


-- MODEL
type alias Model = 
    { notes: Set.Set Note
    , noteRange: (Int, Int)
    , interactive: Bool
    }

type alias Note = Int

initialModel : Model
-- One octave keyboard, starting from C4
initialModel = Model Set.empty (36, 60) True


-- UPDATE

type Msg
    = KeyUp Note
    | KeyDown Note

update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyUp note ->
            if model.interactive then
                { model | notes = Set.remove note model.notes }
            else
                model

        KeyDown note ->
            if model.interactive then
                { model | notes = Set.insert note model.notes }
            else
                model


-- VIEW
view : Model -> Html Msg
view model =
    let
        container inner =
            div [class "piano"]
                [div [class "piano-container"]
                     [div [class "piano-keys"] inner]
                ]
        myStyle = node "style" [] [text PianoStyle.css]

        range = [fst model.noteRange .. snd model.noteRange]
    in
        span [] [myStyle
                , container <| List.map2 viewKey
                    range
                    (List.map (flip Set.member model.notes) range)
                ]

viewKey : Note -> Bool -> Html Msg
viewKey note active =
    if isNatural note then
        div
            [ classList [
                ("piano-white", True),
                ("pressed", active)
              ]
              , onMouseDown (KeyDown note)
              , onMouseUp (KeyUp note)
            ] 
            []
    else
        div 
            [class "piano-black"] 
            [ div [ 
                classList [
                    ("piano-black-raised", True),
                    ("pressed", active)
                ],
                onMouseDown (KeyDown note),
                onMouseUp (KeyUp note)
              ] [] ]


-- Note helpers

octave : Note -> Int
octave note =
    note // 12

isNatural : Note -> Bool
isNatural note =
    List.member (note % 12) [0, 2, 4, 5, 7, 9, 11]

noteName : Note -> String
noteName note =
    let
        getCharAt n str = String.slice n (n + 1) str
        noteName' = getCharAt (note % 12) "CCDDEFFGGAAB"
        alteration = if isNatural note then "" else "#"
    in
       noteName' ++ alteration ++ toString (octave note)
