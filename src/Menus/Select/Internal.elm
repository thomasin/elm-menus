module Menus.Select.Internal exposing (Config, SelectAction(..), Selected(..), selected)

import Menus.Focus
import Menus.Select


{-| to-do
-}
type Selected value
    = SelectChanged SelectAction
    | SelectedSpecific value
    | SelectedFocussed


{-| to-do
-}
type SelectAction
    = MovedLeft
    | MovedRight
    | Cleared { selectionStart : Int, selectionEnd : Int }


{-| to-do
-}
type alias Config selection value =
    { selectChange : SelectAction -> Menus.Select.Change selection
    , selectValue : value -> selection
    , currentlyFocussed : Menus.Focus.Focus value
    }


{-| to-do
-}
selected : Selected value -> Config selection value -> Maybe selection
selected msg config =
    case msg of
        SelectedSpecific value ->
            Just (config.selectValue value)

        SelectChanged action ->
            case config.selectChange action of
                Menus.Select.ChangedTo selection ->
                    Just selection

                Menus.Select.NotChanged ->
                    Nothing

        SelectedFocussed ->
            case config.currentlyFocussed of
                Menus.Focus.On value ->
                    Just (config.selectValue value)

                Menus.Focus.Lost ->
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
