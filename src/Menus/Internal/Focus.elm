module Menus.Internal.Focus exposing (Focussed(..), Focus(..), fromMaybe, toMaybe, scrollIntoView, Config, focussed, keyEvents, loseOnMouseLeave, focusOnMouseMove)

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


scrollIntoView : String -> String -> Maybe Menus.Internal.Base.Direction -> { msgConfig | onNoOp : msg } -> Cmd msg
scrollIntoView optionId ulId maybeDirection msgConfig =
    Browser.Dom.getElement optionId
        |> Task.andThen (\opt ->
            Browser.Dom.getViewportOf ulId
                |> Task.andThen (\{ viewport } ->
                    Browser.Dom.getElement ulId
                        |> Task.andThen (\ul ->
                            let elementY = opt.element.y - ul.element.y in
                            if elementY > viewport.y && (elementY + opt.element.height) < (viewport.y + viewport.height) then
                                -- The element is visible on screen, no need to scroll anything
                                Task.succeed ()

                            else
                                -- We need to scroll the element into view
                                Browser.Dom.setViewportOf ulId 0 elementY
                        )
                )
        )
        |> Task.attempt (\_ -> msgConfig.onNoOp)


type alias Config state value =
    { focusChange : Menus.Internal.Base.Direction -> Maybe value
    , updateFocus : Focus value -> state
    , valueToId : value -> String
    , optionContainerId : String
    }


focussed : Focussed value -> state -> { msgConfig | onNoOp : msg } -> Config state value -> ( state, Cmd msg )
focussed msg state msgConfig config =
    case msg of
        FocussedSpecific value ->
            ( config.updateFocus (HasFocus value)
            , scrollIntoView (config.valueToId value) config.optionContainerId Nothing msgConfig
            )

        FocussedChanged direction ->
            case config.focusChange direction of
                Just newFocus ->
                    ( config.updateFocus (HasFocus newFocus)
                    , scrollIntoView (config.valueToId newFocus) config.optionContainerId (Just direction) msgConfig
                    )

                Nothing ->
                    ( state
                    , Cmd.none
                    )

        FocusLost ->
            ( config.updateFocus NoFocus
            , Cmd.none
            )


keyEvents : { msgConfig | onFocussed : Focussed value -> msg } -> Json.Decode.Decoder ( msg, Menus.Internal.KeyEvent.Opts )
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


focusOnMouseMove : { msgConfig | onFocussed : Focussed value -> msg } -> value -> Html.Attribute msg
focusOnMouseMove msgConfig value =
    Html.Events.on "mousemove" (Json.Decode.succeed (msgConfig.onFocussed (FocussedSpecific value)))

