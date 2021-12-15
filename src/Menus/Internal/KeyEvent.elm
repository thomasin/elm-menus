module Menus.Internal.KeyEvent exposing (Opts, backspace, charKey, down, enter, escape, left, onKeyDown, right, up, tabWith)

import Html
import Html.Events as Events
import Json.Decode


onKeyDown : List (Json.Decode.Decoder ( msg, Opts )) -> Html.Attribute msg
onKeyDown decoders =
    Events.custom "keydown"
        (Json.Decode.oneOf decoders
            |> Json.Decode.map
                (\( msg, opts ) ->
                    { message = msg
                    , stopPropagation = opts.stopPropagation
                    , preventDefault = opts.preventDefault
                    }
                )
        )


type alias Opts =
    { stopPropagation : Bool, preventDefault : Bool }


strictOpts : Opts
strictOpts =
    { stopPropagation = True, preventDefault = True }


enter : msg -> Json.Decode.Decoder ( msg, Opts )
enter msg =
    succeedForKey "Enter" msg strictOpts


tabWith : msg -> Opts -> Json.Decode.Decoder ( msg, Opts )
tabWith msg opts =
    succeedForKey "Tab" msg opts


escape : msg -> Json.Decode.Decoder ( msg, Opts )
escape msg =
    succeedForKey "Escape" msg strictOpts


backspace : msg -> Json.Decode.Decoder ( msg, Opts )
backspace msg =
    succeedForKey "Backspace" msg strictOpts


up : msg -> Json.Decode.Decoder ( msg, Opts )
up msg =
    succeedForKey "ArrowUp" msg strictOpts


down : msg -> Json.Decode.Decoder ( msg, Opts )
down msg =
    succeedForKey "ArrowDown" msg strictOpts


left : msg -> Json.Decode.Decoder ( msg, Opts )
left msg =
    succeedForKey "ArrowLeft" msg strictOpts


right : msg -> Json.Decode.Decoder ( msg, Opts )
right msg =
    succeedForKey "ArrowRight" msg strictOpts


charKey : (String -> msg) -> Json.Decode.Decoder ( msg, Opts )
charKey msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\pressedKey ->
                if pressedKey == " " then
                    Json.Decode.fail "We ignore space presses - they're used to press buttons"

                else if String.length pressedKey == 1 then
                    Json.Decode.succeed ( msg pressedKey, strictOpts )

                else
                    Json.Decode.fail "Not a char key"
            )


succeedForKey : String -> msg -> Opts -> Json.Decode.Decoder ( msg, Opts )
succeedForKey key msg opts =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\pressedKey ->
                if pressedKey == key then
                    Json.Decode.succeed ( msg, opts )

                else
                    Json.Decode.fail "Not the right key"
            )
