module Piano.TouchEvents exposing (..)

{-| Module used to detect which key was affected by some touch event

This is a bit tricky to do right because I don't have direct access to the
document.elementFromPoint function. I'll have to make my own version of it
so the user won't have to use any ports nor native code.

I based on this stack overflow question to write the module:
<https://stackoverflow.com/questions/36169470/multi-touch-for-html5-canvas>

-}

import Array exposing (Array)
import Json.Decode as Json exposing (..)
import Html.Styled
import Html.Styled.Events exposing (onWithOptions, defaultOptions)
import Piano.Note exposing (..)


type alias TouchEvent =
    { name : String

    -- I only need the changedTouches property
    , touches : List Touch
    , targetCoordinates : ( Int, Int )
    , targetWidth : Int
    , targetHeight : Int
    }


type alias Touch =
    { identifier : String
    , coordinates : ( Int, Int )
    }


eventDecoder : Decoder TouchEvent
eventDecoder =
    Json.map5 TouchEvent
        (field "type" string)
        -- (at [ "changedTouches", "0" ] touchDecoder |> map List.singleton)
        (at [ "changedTouches", "0" ] touchDecoder
            |> map List.singleton
        )
        (map2
            (,)
            (at [ "target", "offsetLeft" ] int)
            (at [ "target", "offsetTop" ] int)
        )
        (at [ "target", "offsetWidth" ] int)
        (at [ "target", "offsetHeight" ] int)


touchDecoder : Decoder Touch
touchDecoder =
    Json.map2 Touch
        (field "identifier" int |> map toString)
        (map2
            (,)
            (field "pageX" int)
            (field "pageY" int)
        )



-- strangeList : Decoder a -> Decoder (List a)
-- strangeList d =
--     let
--         innerDecoder : ( Int, a ) -> Decoder (List a)
--         innerDecoder (n, a) =
--             let
--                 ffds
--     in
--         map2 (,)
--             (field "length" int |> map (\x -> x-1))
--             d
--             |> andThen innerDecoder


debugDecoderErrors : Decoder a -> Decoder a
debugDecoderErrors d =
    let
        fromResult : Result String a -> Decoder a
        fromResult r =
            case r of
                Result.Err m ->
                    fail (Debug.log "error decoding" m)

                Ok a ->
                    succeed (Debug.log "decoded" a)
    in
        value
            |> map (decodeValue d)
            |> andThen fromResult



-- event =
--     TouchEvent
--         ""
--         []
--         ( 0, 0 )


onTouchStart : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchStart msg =
    onWithOptions
        "touchstart"
        { defaultOptions | preventDefault = True }
        (Json.map msg eventDecoder)


onTouchMove : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchMove msg =
    onWithOptions
        "touchmove"
        { defaultOptions | preventDefault = True }
        (Json.map msg eventDecoder)


onTouchEnd : (TouchEvent -> msg) -> Html.Styled.Attribute msg
onTouchEnd msg =
    onWithOptions
        "touchend"
        { defaultOptions | preventDefault = True }
        (Json.map msg eventDecoder)


{-| Stores data that allows to convert from (x, y) coordinates to a
Maybe Note
-}
type alias KeyPositions =
    { naturalKeyStarts : Array ( Note, ( Int, Int ) )
    , naturalKeySize : ( Int, Int )
    }


{-| This function is a bit computionally heavy so it should be called only
on the first recieved event or when an inconsistency is detected (probably
because the piano container changed the position in the document)
-}
computeKeyPositions : Note -> TouchEvent -> KeyPositions
computeKeyPositions note { targetCoordinates, targetWidth, targetHeight } =
    let
        naturalKeyStarts =
            Array.fromList allNotes
                |> Array.filter isNatural
                |> Array.map
                    (\n ->
                        ( n
                        , ( Tuple.first targetCoordinates
                                - (targetWidth * (naturalKeyDistance note n))
                            -- The Y axis doesn't change between keys
                          , Tuple.second targetCoordinates
                          )
                        )
                    )
    in
        KeyPositions
            naturalKeyStarts
            ( targetWidth, targetHeight )


fromCoordinates : ( Int, Int ) -> KeyPositions -> Maybe Note
fromCoordinates ( x, y ) positions =
    let
        matches : ( Int, Int ) -> Bool
        matches ( startX, startY ) =
            let
                endX =
                    startX + Tuple.first positions.naturalKeySize

                endY =
                    startY + Tuple.second positions.naturalKeySize
            in
                startX <= x && x <= endX && startY <= y && y <= endY
    in
        positions.naturalKeyStarts
            |> Array.filter (Tuple.second >> matches)
            |> Array.map Tuple.first
            |> Array.get 0
