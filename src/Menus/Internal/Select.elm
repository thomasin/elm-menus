module Menus.Internal.Select exposing (Selected(..), Config, selected)

import Html exposing (Html)
import Html.Events
import Browser.Dom
import Task
import Json.Decode

import Menus.Internal.Base
import Menus.Internal.KeyEvent
import Menus.Internal.Focus


type Selected value
    = SelectChanged Menus.Internal.Base.Direction
    | SelectedSpecific value
    | SelectedFocussed


type alias Config selection value =
    { selectChange : Menus.Internal.Base.Direction -> selection
    , selectValue : value -> selection
    , currentlyFocussed : Menus.Internal.Focus.Focus value
    }


selected : Selected value -> Config selection value -> Maybe selection
selected msg config =
    case msg of
        SelectedSpecific value ->
            Just (config.selectValue value)

        SelectChanged direction ->
            Just (config.selectChange direction)

        SelectedFocussed ->
            case config.currentlyFocussed of
                Menus.Internal.Focus.HasFocus value ->
                    Just (config.selectValue value)

                Menus.Internal.Focus.NoFocus ->
                    Nothing


--keyEvents : { msgConfig | onFocussed : Focussed value -> msg } -> Json.Decode.Decoder msg
--keyEvents msgConfig =
--    Json.Decode.oneOf
--        [ Menus.Internal.KeyEvent.up (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Up))
--        , Menus.Internal.KeyEvent.down (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Down))
--        , Menus.Internal.KeyEvent.left (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Left))
--        , Menus.Internal.KeyEvent.right (msgConfig.onFocussed (FocussedChanged Menus.Internal.Base.Right))
--        ]


--loseOnMouseLeave : { msgConfig | onFocussed : Focussed value -> msg } -> Html.Attribute msg
--loseOnMouseLeave msgConfig =
--    Html.Events.onMouseLeave (msgConfig.onFocussed FocusLost)


--focusOnMouseOver : { msgConfig | onFocussed : Focussed value -> msg } -> value -> Html.Attribute msg
--focusOnMouseOver msgConfig value =
--    Html.Events.onMouseOver (msgConfig.onFocussed (FocussedSpecific value))


















