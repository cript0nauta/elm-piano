module Piano.TouchEvents exposing (ElementData, Touch, TouchEvent, collection, combine, eventDecoder, fromResult, impureCollection, onTouchEnd, onTouchMove, onTouchStart, touchDecoder)

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

    -- I only need the changedTouches property
    , touches : List Touch

    -- , keys : List ElementData
    }


type alias Touch =
    { identifier : String
    , coordinates : ( Float, Float )
    }


type alias ElementData =
    { note : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


eventDecoder : Decoder TouchEvent
eventDecoder =
    Json.map2 TouchEvent
        (field "type" string)
        (field "changedTouches" (collection touchDecoder))



-- (at [ "target", "attributes", "data-note", "value" ] string
--     |> map String.toInt
--     |> andThen fromResult
--     |> andThen
--         (\note ->
--             at
--                 (if isNatural note then
--                     [ "target", "parentNode", "childNodes" ]
--                  else
--                     [ "target", "parentNode", "parentNode", "childNodes" ]
--                 )
--                 (collection elementDecoder)
--         )
-- )


touchDecoder : Decoder Touch
touchDecoder =
    Json.map2 Touch
        (field "identifier" int |> map String.fromInt)
        (map2
            Tuple.pair
            (field "pageX" float)
            (field "pageY" float)
        )



-- debugDecoderErrors : Decoder a -> Decoder a
-- debugDecoderErrors d =
--     let
--         fromResult_ : Result String a -> Decoder a
--         fromResult_ r =
--             case r of
--                 Result.Err m ->
--                     fail (Debug.log "error decoding" m)
--                 Ok a ->
--                     succeed (Debug.log "decoded" a)
--     in
--         value
--             |> map (decodeValue d)
--             |> andThen fromResult_
-- elementDecoder : Decoder ElementData
-- elementDecoder =
--     oneOf
--         [ at [ "attributes", "data-note", "value" ] string
--         , at [ "childNodes", "0", "attributes", "data-note", "value" ] string
--         ]
--         |> map String.toInt
--         |> andThen fromResult
--         |> andThen
--             (\note ->
--                 let
--                     childField : String -> Decoder a -> Decoder a
--                     childField fieldName dec =
--                         if isNatural note then
--                             field fieldName dec
--                         else
--                             at ([ "childNodes", "0", fieldName ]) dec
--                 in
--                     map4 (ElementData note)
--                         (field "offsetLeft" int
--                             |> map
--                                 (\left ->
--                                     if isNatural note then
--                                         left
--                                     else
--                                         -- Black keys have a left: -10px CSS property
--                                         -- needs to be compensated
--                                         left - 10
--                                 )
--                         )
--                         (field "offsetTop" int)
--                         (childField "offsetWidth" int)
--                         (childField "offsetHeight" int)
--             )


impureCollection : Decoder a -> Decoder (List a)
impureCollection d =
    collection
        (oneOf
            [ map Just d
            , succeed Nothing
            ]
        )
        |> map (List.filterMap identity)


fromResult : Result String value -> Decoder value
fromResult r =
    case r of
        Err msg ->
            fail msg

        Ok val ->
            succeed val


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



-- fromCoordinates : TouchEvent -> ( Int, Int ) -> Maybe Int
-- fromCoordinates evt ( x, y ) =
--     let
--         matches : ElementData -> Bool
--         matches e =
--             (e.x <= x && x <= (e.x + e.width) && e.y <= y && y <= (e.y + e.height))
--     in
--         evt.keys
--             |> List.partition (isNatural << .note)
--             |> (\( naturals, alters ) -> alters ++ naturals)
--             |> List.filter matches
--             |> List.head
--             |> Maybe.map .note


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
