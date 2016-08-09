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
    , notesRange: (Int, Int)
    }

type alias Note = Int

initialModel : Model
-- One octave keyboard, starting from C4
initialModel = Model Set.empty (36, 60)


-- UPDATE

type Msg
    = A

update : Msg -> Model -> Model
update msg model =
    case msg of
        A ->
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
    in
        span [] [myStyle
                , container <| List.map viewKey
                    [fst model.notesRange .. snd model.notesRange]
                ]

viewKey : Note -> Html Msg
viewKey note =
    if isNatural note then
        div [class "piano-white"] []
    else
        div [class "piano-black"] [ div [class "piano-black-raised" ] [] ]


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
