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
    , showSizeSelector: Bool
    , debugNotes: Bool
    }

type alias Note = Int

initialModel : Model
-- One octave keyboard, starting from C4
initialModel =
    { notes = Set.empty
    , noteRange = keyboard25Keys
    , interactive = True
    , showSizeSelector = True
    , debugNotes = True
    }

keyboard12Keys = (48, 59)
keyboard25Keys = (36, 60)
keyboard49Keys = (24, 72)
keyboard61Keys = (24, 84)
keyboard76Keys = (16, 91)
keyboard88Keys = (9, 96)

-- UPDATE

type Msg
    = KeyUp Note
    | KeyDown Note
    | ChangeNoteRange (Int, Int)

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

        ChangeNoteRange size ->
            { model | noteRange = size }


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

        sizeSelector =
            if model.showSizeSelector then
               let
                   keyboardOption size =
                       let
                           keys = snd size - fst size + 1
                       in
                           button [onClick (ChangeNoteRange size)]
                                  [text (toString keys ++ "-key piano")]
               in
                  List.map keyboardOption
                      [ keyboard12Keys
                      , keyboard25Keys
                      , keyboard49Keys
                      , keyboard61Keys
                      , keyboard76Keys
                      , keyboard88Keys]
                    |> List.intersperse (br [] [])
           else
               []

        debugNotes =
            if model.debugNotes then
                [ div [] [text <| "Currently pressed notes: " ++ String.join ", "
                    (model.notes |> Set.toList |> List.map noteName)
                    ] ]
            else
                []
    in
        span [] ([myStyle
                , container <| List.map2 viewKey
                    range
                    (List.map (flip Set.member model.notes) range)
                ] ++ debugNotes ++ sizeSelector)

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
