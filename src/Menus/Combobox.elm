module Menus.Combobox exposing
    ( State, init, opened, closed, isOpen
    , Config, Emptiable, emptiable, MsgConfig
    , Focussed, Inputted, Selected, inputted, selected, focussed
    , Token, menuToken, input, options, optionsDiv, option
    )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.


# State

@docs State, init, opened, closed, isOpen


# Config

@docs Config, Emptiable, emptiable, MsgConfig


# Msgs

@docs Focussed, Inputted, Selected, inputted, selected, focussed


# Views

@docs Token, menuToken, input, options, optionsDiv, option

-}

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Menus.Active
import Menus.Combobox.Internal
import Menus.Focus
import Menus.Focus.Internal
import Menus.KeyEvent.Internal
import Menus.Select.Internal


{-| to-do

-}
type alias Focussed value =
    Menus.Focus.Internal.Focussed value


{-| to-do

-}
type alias Selected value =
    Menus.Select.Internal.Selected value


{-| to-do

-}
type alias Config options option value selected =
    Menus.Combobox.Internal.Config options option value selected


{-| to-do

-}
type alias Emptiable options option value selected =
    Menus.Combobox.Internal.EmptiableConfig options option value selected


{-| to-do

-}
type Inputted
    = InputChanged String


{-| to-do

-}
emptiable : Emptiable options option value selected -> Config options option value selected
emptiable config =
    Menus.Combobox.Internal.Emptiable config


{-| to-do

-}
type alias OpenState value =
    { input : String
    , lastMatch : Maybe value
    , focus : Menus.Focus.Focus value
    }


{-| The state of the combobox. Holds internal state about the value of the input & current focus
but does not manageavailable options or current selection.
-}
type State value
    = Opened (OpenState value)
    | Closed


{-| Initialises a closed combobox
-}
init : State value
init =
    Closed


{-| Called when the combobox needs to be closed
-}
closed : Token options option value selected msg -> ( State value, Cmd msg )
closed _ =
    ( Closed, Cmd.none )


{-| Called when the combobox needs to be opened
-}
opened : Token options option value selected msg -> ( State value, Cmd msg )
opened token =
    case token.state of
        Opened openState ->
            ( Opened openState, Cmd.none )

        Closed ->
            ( Opened
                { input =
                    Menus.Combobox.Internal.selectionToLabel token.config token.selected
                , lastMatch =
                    case Menus.Combobox.Internal.selectionToOption token.config token.selected token.options of
                        Just option_ ->
                            Just (Menus.Combobox.Internal.optionToValue token.config option_)

                        Nothing ->
                            Nothing
                , focus =
                    case Menus.Combobox.Internal.selectionToOption token.config token.selected token.options of
                        Just option_ ->
                            Menus.Focus.On (Menus.Combobox.Internal.optionToValue token.config option_)

                        Nothing ->
                            Menus.Focus.Lost
                }
            , Menus.Combobox.Internal.focusOnControl (Menus.Combobox.Internal.id token.config) token.msgConfig.onNoOp
            )



-- {-| All visible options for a combobox. This is an important function. If you want to change
-- which options are visible, or in which order they are presented, those changes should be made in the
-- .visibleOptions field of the [Config](#Config) record.
-- -}


{-| Called when a Focus msg is received
-}
focussed : Focussed value -> Token options option value selected msg -> ( State value, Cmd msg )
focussed msg token =
    let
        config : Menus.Focus.Internal.Config (State value) value
        config =
            { focusChange =
                \direction ->
                    case token.state of
                        Opened openState ->
                            let
                                visibleOptions_ : options
                                visibleOptions_ =
                                    case token.config of
                                        Menus.Combobox.Internal.Emptiable emptiableConfig ->
                                            emptiableConfig.visibleOptions openState.input token.selected token.options
                                                |> Maybe.map Tuple.second
                                                |> Maybe.withDefault emptiableConfig.empty
                            in
                            case Menus.Combobox.Internal.focusChange token.config direction openState.focus visibleOptions_ of
                                Menus.Focus.On opt ->
                                    Menus.Focus.On (Menus.Combobox.Internal.optionToValue token.config opt)

                                Menus.Focus.Lost ->
                                    Menus.Focus.Lost

                        Closed ->
                            Menus.Focus.Lost
            , updateFocus =
                \newFocus ->
                    case token.state of
                        Opened openState ->
                            case newFocus of
                                Menus.Focus.On value ->
                                    Opened { openState | lastMatch = Just value, focus = Menus.Focus.On value }

                                Menus.Focus.Lost ->
                                    Opened { openState | focus = Menus.Focus.Lost }

                        Closed ->
                            Closed
            , valueToId = \value -> Menus.Combobox.Internal.ids.option (Menus.Combobox.Internal.valueToString token.config value) (Menus.Combobox.Internal.id token.config)
            , optionContainerId = Menus.Combobox.Internal.ids.options (Menus.Combobox.Internal.id token.config)
            }
    in
    Menus.Focus.Internal.focussed msg token.state token.msgConfig config


{-| Called when a Select msg is received
-}
selected : Selected value -> Token options option value selected msg -> ( selected, State value, Cmd msg )
selected msg token =
    let
        config : Menus.Select.Internal.Config selected value
        config =
            { selectChange = \action -> Menus.Combobox.Internal.selectChange token.config action token.selected token.options
            , selectValue = \value -> Menus.Combobox.Internal.selectValue token.config value token.selected token.options
            , currentlyFocussed =
                case token.state of
                    Opened openState ->
                        openState.focus

                    Closed ->
                        Menus.Focus.Lost
            }

        makeSelectionChange : Maybe selected -> State value -> Cmd msg -> ( selected, State value, Cmd msg )
        makeSelectionChange maybeSelection state cmd =
            case maybeSelection of
                Just selection ->
                    ( selection, Closed, Menus.Combobox.Internal.focusOnControl (Menus.Combobox.Internal.id token.config) token.msgConfig.onNoOp )

                Nothing ->
                    ( token.selected, state, cmd )
    in
    case msg of
        Menus.Select.Internal.SelectedSpecific _ ->
            makeSelectionChange (Menus.Select.Internal.selected msg config) token.state Cmd.none

        Menus.Select.Internal.SelectedFocussed ->
            makeSelectionChange (Menus.Select.Internal.selected msg config) token.state Cmd.none

        Menus.Select.Internal.SelectChanged (Menus.Select.Internal.Cleared inputSelection) ->
            let
                deleteSelectedInput : String -> String
                deleteSelectedInput str =
                    if inputSelection.selectionStart == inputSelection.selectionEnd then
                        String.slice 0 (inputSelection.selectionStart - 1) str ++ String.dropLeft inputSelection.selectionEnd str

                    else
                        String.slice 0 inputSelection.selectionStart str ++ String.dropLeft inputSelection.selectionEnd str

                -- APPLES start: 2, end: 3
            in
            case token.state of
                Opened openState ->
                    case openState.input of
                        "" ->
                            makeSelectionChange (Menus.Select.Internal.selected msg config) token.state Cmd.none

                        _ ->
                            inputted (InputChanged (deleteSelectedInput openState.input)) token

                Closed ->
                    case Menus.Select.Internal.selected msg config of
                        Just selection ->
                            makeSelectionChange (Just selection) token.state Cmd.none

                        Nothing ->
                            inputted (InputChanged (deleteSelectedInput (Menus.Combobox.Internal.selectionToLabel token.config token.selected))) token

        Menus.Select.Internal.SelectChanged _ ->
            makeSelectionChange (Menus.Select.Internal.selected msg config) token.state Cmd.none


{-| Called when an Input msg is received
-}
inputted : Inputted -> Token options option value selected msg -> ( selected, State value, Cmd msg )
inputted msg token =
    case msg of
        InputChanged str ->
            let
                activeOption : Maybe (Menus.Active.Active option)
                activeOption =
                    case token.config of
                        Menus.Combobox.Internal.Emptiable emptiableConfig ->
                            emptiableConfig.visibleOptions str token.selected token.options
                                |> Maybe.map Tuple.first
            in
            case activeOption of
                Just (Menus.Active.FocussedAndSelected match) ->
                    ( Menus.Combobox.Internal.selectValue token.config (Menus.Combobox.Internal.optionToValue token.config match) token.selected token.options
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (Menus.Combobox.Internal.optionToValue token.config match)
                        , focus = Menus.Focus.On (Menus.Combobox.Internal.optionToValue token.config match)
                        }
                    , Cmd.none
                    )

                Just (Menus.Active.Focussed match) ->
                    ( token.selected
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (Menus.Combobox.Internal.optionToValue token.config match)
                        , focus = Menus.Focus.On (Menus.Combobox.Internal.optionToValue token.config match)
                        }
                    , Cmd.none
                    )

                Nothing ->
                    case token.state of
                        Opened openState ->
                            ( token.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = openState.lastMatch
                                , focus = Menus.Focus.Lost
                                }
                            , Cmd.none
                            )

                        Closed ->
                            ( token.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = Nothing
                                , focus = Menus.Focus.Lost
                                }
                            , Cmd.none
                            )


{-| Map your msgs to actions. Most Preset modules should provide
this for you.
-}
type alias MsgConfig value msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Focussed value -> msg
    , onSelected : Selected value -> msg
    , onInput : Inputted -> msg
    , onNoOp : msg
    }


{-| Helper function to check whether the menu is open or not.
-}
isOpen : State value -> Bool
isOpen state =
    case state of
        Opened _ ->
            True

        Closed ->
            False



-- Views --


{-| This token is used for all view functions.
-}
type alias Token options option value selected msg =
    { state : State value
    , config : Config options option value selected
    , msgConfig : MsgConfig value msg
    , options : options
    , visibleOptions : Maybe options
    , selected : selected
    , focussed : Maybe value
    }


{-| Create a token! This is used for all view functions.
-}
menuToken : { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> Token options option value selected msg
menuToken args =
    let
        visibleOptions_ : Maybe options
        visibleOptions_ =
            case args.state of
                Opened openState ->
                    case args.config of
                        Menus.Combobox.Internal.Emptiable emptiableConfig ->
                            emptiableConfig.visibleOptions openState.input args.selected args.options
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault emptiableConfig.empty
                                |> Just

                Closed ->
                    Nothing

        focussed_ : Maybe value
        focussed_ =
            case args.state of
                Opened openState ->
                    Menus.Focus.Internal.toMaybe openState.focus

                Closed ->
                    Nothing
    in
    { state = args.state
    , config = args.config
    , msgConfig = args.msgConfig
    , options = args.options
    , visibleOptions = visibleOptions_
    , selected = args.selected
    , focussed = focussed_
    }


{-| The main control for the combobox (:
-}
input : Token options option value selected msg -> { placeholder : String } -> List (Html.Attribute msg) -> Html msg
input token args attributes =
    case token.state of
        Opened openState ->
            let
                activeDescendant : String
                activeDescendant =
                    Maybe.map (Menus.Combobox.Internal.valueToString token.config) token.focussed
                        |> Maybe.map (Menus.Combobox.Internal.ids.option (Menus.Combobox.Internal.id token.config))
                        |> Maybe.withDefault ""
            in
            Html.input
                (List.append attributes
                    [ Attr.id (Menus.Combobox.Internal.ids.control (Menus.Combobox.Internal.id token.config))
                    , Aria.controls [ Menus.Combobox.Internal.ids.options (Menus.Combobox.Internal.id token.config) ]
                    , Attr.type_ "text"
                    , Attr.autocomplete False
                    , Attr.spellcheck False
                    , Attr.placeholder args.placeholder
                    , Attr.value openState.input
                    , Aria.hasListBoxPopUp
                    , Aria.activeDescendant activeDescendant
                    , Aria.expanded True
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onInput (token.msgConfig.onInput << InputChanged)
                    , Menus.KeyEvent.Internal.onKeyDown
                        [ Menus.KeyEvent.Internal.escape token.msgConfig.onClosed
                        , Menus.KeyEvent.Internal.enter (token.msgConfig.onSelected Menus.Select.Internal.SelectedFocussed)
                        , Menus.KeyEvent.Internal.backspace (token.msgConfig.onSelected << Menus.Select.Internal.SelectChanged << Menus.Select.Internal.Cleared)
                        , Menus.KeyEvent.Internal.right (token.msgConfig.onSelected Menus.Select.Internal.SelectedFocussed)
                        , Menus.Focus.Internal.keyEvents token.msgConfig
                        ]
                    ]
                )
                []

        Closed ->
            Html.input
                (List.append attributes
                    [ Attr.id (Menus.Combobox.Internal.ids.control (Menus.Combobox.Internal.id token.config))
                    , Aria.controls [ Menus.Combobox.Internal.ids.options (Menus.Combobox.Internal.id token.config) ]
                    , Attr.type_ "text"
                    , Attr.autocomplete False
                    , Attr.spellcheck False
                    , Attr.placeholder args.placeholder
                    , Attr.value (Menus.Combobox.Internal.selectionToLabel token.config token.selected)
                    , Aria.hasListBoxPopUp
                    , Aria.expanded False
                    , Events.onInput (token.msgConfig.onInput << InputChanged)
                    , Events.onMouseDown token.msgConfig.onOpened
                    , Menus.KeyEvent.Internal.onKeyDown
                        [ Menus.KeyEvent.Internal.left (token.msgConfig.onSelected (Menus.Select.Internal.SelectChanged Menus.Select.Internal.MovedLeft))
                        , Menus.KeyEvent.Internal.right (token.msgConfig.onSelected (Menus.Select.Internal.SelectChanged Menus.Select.Internal.MovedRight))
                        , Menus.KeyEvent.Internal.backspace (token.msgConfig.onSelected << Menus.Select.Internal.SelectChanged << Menus.Select.Internal.Cleared)
                        , Menus.KeyEvent.Internal.down token.msgConfig.onOpened
                        , Menus.KeyEvent.Internal.up token.msgConfig.onOpened
                        ]
                    ]
                )
                []


{-| A container for your list of options!
This is a `ul` node and means that [#option](#option) should be direct children
-}
options : Token options option value selected msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
options token attributes =
    Html.ul
        (List.append attributes
            [ Attr.id (Menus.Combobox.Internal.ids.options (Menus.Combobox.Internal.id token.config))
            , Role.listBox
            , Key.tabbable False
            , Menus.Focus.Internal.loseOnMouseLeave token.msgConfig
            ]
        )


{-| A container for your list of options!
This is a `div` node and is useful if you want your options list to be nested, or
-}
optionsDiv : Token options option value selected msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
optionsDiv token attributes =
    Html.div
        (List.append attributes
            [ Attr.id (Menus.Combobox.Internal.ids.options (Menus.Combobox.Internal.id token.config))
            , Role.listBox
            , Key.tabbable False
            , Menus.Focus.Internal.loseOnMouseLeave token.msgConfig
            ]
        )


{-| Use this to wrap every option you want to be focus and selectable.
This is a `li` tag so must be nested in a list, like [#options](#options)
-}
option : Token options option value selected msg -> { value : value, isSelected : Bool } -> List (Html.Attribute msg) -> (List (Html Never) -> Html msg)
option token args attributes children =
    List.map (Html.map never) children
        |> Html.li
            (List.append attributes
                [ Attr.id (Menus.Combobox.Internal.ids.option (Menus.Combobox.Internal.valueToString token.config args.value) (Menus.Combobox.Internal.id token.config))
                , Role.option
                , Aria.selected args.isSelected
                , Key.tabbable False
                , Menus.Focus.Internal.focusOnMouseMove token.msgConfig token.focussed args.value
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Events.onClick (token.msgConfig.onSelected (Menus.Select.Internal.SelectedSpecific args.value))
                ]
            )
