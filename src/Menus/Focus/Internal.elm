module Menus.Focus.Internal exposing (Config, Focussed(..), focusOnMouseMove, focussed, keyEvents, loseOnMouseLeave, toMaybe)

import Browser.Dom
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Menus.Focus
import Menus.KeyEvent.Internal
import Task


type Focussed value
    = FocussedChanged Menus.Focus.FocusAction
    | FocussedSpecific value
    | FocusLost


toMaybe : Menus.Focus.Focus value -> Maybe value
toMaybe focus =
    case focus of
        Menus.Focus.On value ->
            Just value

        Menus.Focus.Lost ->
            Nothing


scrollIntoView : String -> String -> { msgConfig | onNoOp : msg } -> Cmd msg
scrollIntoView optionId ulId msgConfig =
    Browser.Dom.getElement optionId
        |> Task.andThen
            (\opt ->
                Browser.Dom.getViewportOf ulId
                    |> Task.andThen
                        (\{ viewport } ->
                            Browser.Dom.getElement ulId
                                |> Task.andThen
                                    (\ul ->
                                        let
                                            -- The distance of the option element top from the ul viewport
                                            elementY : Float
                                            elementY =
                                                opt.element.y - ul.element.y
                                        in
                                        if elementY > 0 && (elementY + opt.element.height) < viewport.height then
                                            -- The element is visible on screen, no need to scroll anything
                                            Task.succeed ()

                                        else if elementY > 0 then
                                            -- The element is below the visible viewport, move it to the bottom
                                            Browser.Dom.setViewportOf ulId 0 ((viewport.y + elementY) - (viewport.height - opt.element.height))

                                        else
                                            -- The element is above the visible viewport, move it to the top
                                            Browser.Dom.setViewportOf ulId 0 (viewport.y + elementY)
                                    )
                        )
            )
        |> Task.attempt (\_ -> msgConfig.onNoOp)


type alias Config state value =
    { focusChange : Menus.Focus.FocusAction -> Menus.Focus.Focus value
    , updateFocus : Menus.Focus.Focus value -> state
    , valueToId : value -> String
    , optionContainerId : String
    }


focussed : Focussed value -> state -> { msgConfig | onNoOp : msg } -> Config state value -> ( state, Cmd msg )
focussed msg state msgConfig config =
    case msg of
        FocussedSpecific value ->
            ( config.updateFocus (Menus.Focus.On value)
            , scrollIntoView (config.valueToId value) config.optionContainerId msgConfig
            )

        FocussedChanged action ->
            case config.focusChange action of
                Menus.Focus.On newFocus ->
                    ( config.updateFocus (Menus.Focus.On newFocus)
                    , scrollIntoView (config.valueToId newFocus) config.optionContainerId msgConfig
                    )

                Menus.Focus.Lost ->
                    ( state
                    , Cmd.none
                    )

        FocusLost ->
            ( config.updateFocus Menus.Focus.Lost
            , Cmd.none
            )


keyEvents : { msgConfig | onFocussed : Focussed value -> msg } -> Json.Decode.Decoder ( msg, Menus.KeyEvent.Internal.Opts )
keyEvents msgConfig =
    Json.Decode.oneOf
        [ Menus.KeyEvent.Internal.up (msgConfig.onFocussed (FocussedChanged Menus.Focus.MovedUp))
        , Menus.KeyEvent.Internal.down (msgConfig.onFocussed (FocussedChanged Menus.Focus.MovedDown))
        , Menus.KeyEvent.Internal.left (msgConfig.onFocussed (FocussedChanged Menus.Focus.MovedLeft))
        , Menus.KeyEvent.Internal.right (msgConfig.onFocussed (FocussedChanged Menus.Focus.MovedRight))
        ]


loseOnMouseLeave : { msgConfig | onFocussed : Focussed value -> msg } -> Html.Attribute msg
loseOnMouseLeave msgConfig =
    Html.Events.onMouseLeave (msgConfig.onFocussed FocusLost)


focusOnMouseMove : { msgConfig | onFocussed : Focussed value -> msg } -> Maybe value -> value -> Html.Attribute msg
focusOnMouseMove msgConfig currentFocus value =
    if currentFocus == Just value then
        -- We don't need to trigger any more mouse move events
        Html.Attributes.class ""

    else
        Html.Events.on "mousemove" (Json.Decode.succeed (msgConfig.onFocussed (FocussedSpecific value)))
