module Piano.TouchEvents exposing
    ( Touch
    , TouchEvent
    , onTouchEnd
    , onTouchMove
    , onTouchStart
    )

{-| Module used to detect which key was affected by some touch event

This is a bit tricky to do right because I don't have direct access to the
document.elementFromPoint function. I'll have to make my own version of it
so the user won't have to use any ports nor native code.

I based on this stack overflow question to write the module:
<https://stackoverflow.com/questions/36169470/multi-touch-for-html5-canvas>

-}

import Array exposing (Array)
import Html.Styled
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as Json exposing (..)


type alias TouchEvent =
    { name : String
    , touches : List Touch
    }


type alias Touch =
    { identifier : String
    , coordinates : ( Float, Float )
    }


eventDecoder : Decoder TouchEvent
eventDecoder =
    Json.map2 TouchEvent
        (field "type" string)
        (field "changedTouches" (collection touchDecoder))


touchDecoder : Decoder Touch
touchDecoder =
    Json.map2 Touch
        (field "identifier" int |> map String.fromInt)
        (map2
            Tuple.pair
            (field "pageX" float)
            (field "pageY" float)
        )


onTouchStart : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchStart msg =
    preventDefaultOn
        "touchstart"
        (Json.map (\e -> ( msg e, True )) eventDecoder)


onTouchMove : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchMove msg =
    preventDefaultOn
        "touchmove"
        (Json.map (\e -> ( msg e, True )) eventDecoder)


onTouchEnd : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchEnd msg =
    preventDefaultOn
        "touchend"
        (Json.map (\e -> ( msg e, True )) eventDecoder)


collection : Decoder a -> Decoder (List a)
collection decoder =
    field "length" int
        |> andThen
            (\length ->
                List.range 0 (length - 1)
                    |> List.map (\index -> field (String.fromInt index) decoder)
                    |> combine
            )


combine : List (Decoder a) -> Decoder (List a)
combine =
    List.foldr (map2 (::)) (succeed [])
