module Menus.Internal.Focus exposing (Focussed(..), Focus(..), fromMaybe, toMaybe, attemptFocus, Config, focussed, keyEvents, loseOnMouseLeave, focusOnMouseOver)

import Html exposing (Html)
import Html.Events
import Browser.Dom
import Task
import Json.Decode

import Menus.Internal.Base
import Menus.Internal.KeyEvent


type Focussed value
    = FocussedChanged Menus.Internal.Base.Direction
    | FocussedSpecific value
    | FocusLost


type Focus value
    = HasFocus value
    | NoFocus


fromMaybe : Maybe value -> Focus value
fromMaybe maybeValue =
    case maybeValue of
        Just value ->
            HasFocus value

        Nothing ->
            NoFocus


toMaybe : Focus value -> Maybe value
toMaybe focus =
    case focus of
        HasFocus value ->
            Just value

        NoFocus ->
            Nothing


attemptFocus : String -> { msgConfig | onNoOp : msg } -> Cmd msg
attemptFocus id msgConfig =
    Task.attempt (always msgConfig.onNoOp) (Browser.Dom.focus id)



type alias Config state value =
    { focusChange : Menus.Internal.Base.Direction -> Maybe value
    , updateFocus : Focus value -> state
    , valueToId : value -> String
    }


focussed : Focussed value -> state -> { msgConfig | onNoOp : msg } -> Config state value -> ( state, Cmd msg )
focussed msg state msgConfig config =
    case msg of
        FocussedSpecific value ->
            ( config.updateFocus (HasFocus value)
            , attemptFocus (config.valueToId value) msgConfig
            )

        FocussedChanged direction ->
            case config.focusChange direction of
                Just newFocus ->
                    ( config.updateFocus (HasFocus newFocus)
                    , attemptFocus (config.valueToId newFocus) msgConfig
                    )

                Nothing ->
                    ( state
                    , Cmd.none
                    )

        FocusLost ->
            ( config.updateFocus NoFocus
            , attemptFocus "" msgConfig
            )


keyEvents : { msgConfig | onFocussed : Focussed value -> msg } -> Json.Decode.Decoder msg
keyEvents msgConfig =
    Json.Decode.oneOf
        [ Menus.Internal.KeyEvent.up (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Up))
        , Menus.Internal.KeyEvent.down (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Down))
        , Menus.Internal.KeyEvent.left (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Left))
        , Menus.Internal.KeyEvent.right (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Right))
        ]


loseOnMouseLeave : { msgConfig | onFocussed : Focussed value -> msg } -> Html.Attribute msg
loseOnMouseLeave msgConfig =
    Html.Events.onMouseLeave (msgConfig.onFocussed FocusLost)


focusOnMouseOver : { msgConfig | onFocussed : Focussed value -> msg } -> value -> Html.Attribute msg
focusOnMouseOver msgConfig value =
    Html.Events.onMouseOver (msgConfig.onFocussed (FocussedSpecific value))


















