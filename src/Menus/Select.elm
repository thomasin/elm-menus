module Menus.Select exposing (Config, Selected(..), Change(..), SelectAction(..), selected)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}
 
import Menus.Internal.Focus
import Menus.Focus


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Selected value
    = SelectChanged SelectAction
    | SelectedSpecific value
    | SelectedFocussed


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type SelectAction
    = MovedLeft
    | MovedRight
    | Cleared


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Change selection
    = ChangedTo selection
    | NotChanged


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Config selection value =
    { selectChange : SelectAction -> Change selection
    , selectValue : value -> selection
    , currentlyFocussed : Menus.Focus.Focus value
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
selected : Selected value -> Config selection value -> Maybe selection
selected msg config =
    case msg of
        SelectedSpecific value ->
            Just (config.selectValue value)

        SelectChanged action ->
            case config.selectChange action of
                ChangedTo selection ->
                    Just selection

                NotChanged ->
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

