module Menus.Internal.KeyEvent exposing (backspace, charKey, down, enter, escape, left, onKeyDown, right, up)

import Html
import Html.Events as Events
import Json.Decode


onKeyDown : List (Json.Decode.Decoder msg) -> Html.Attribute msg
onKeyDown decoders =
    Events.custom "keydown"
        (Json.Decode.oneOf decoders
            |> Json.Decode.map
                (\msg ->
                    { message = msg
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )


enter : msg -> Json.Decode.Decoder msg
enter msg =
    succeedForKey "Enter" msg


escape : msg -> Json.Decode.Decoder msg
escape msg =
    succeedForKey "Escape" msg


backspace : msg -> Json.Decode.Decoder msg
backspace msg =
    succeedForKey "Backspace" msg


up : msg -> Json.Decode.Decoder msg
up msg =
    succeedForKey "ArrowUp" msg


down : msg -> Json.Decode.Decoder msg
down msg =
    succeedForKey "ArrowDown" msg


left : msg -> Json.Decode.Decoder msg
left msg =
    succeedForKey "ArrowLeft" msg


right : msg -> Json.Decode.Decoder msg
right msg =
    succeedForKey "ArrowRight" msg


charKey : (String -> msg) -> Json.Decode.Decoder msg
charKey msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\pressedKey ->
                if pressedKey == " " then
                    Json.Decode.fail "We ignore space presses - they're used to press buttons"

                else if String.length pressedKey == 1 then
                    Json.Decode.succeed (msg pressedKey)

                else
                    Json.Decode.fail "Not a char key"
            )


succeedForKey : String -> msg -> Json.Decode.Decoder msg
succeedForKey key msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\pressedKey ->
                if pressedKey == key then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "Not the right key"
            )
