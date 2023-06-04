module Menus.Combobox exposing (Config, Focussed, Inputted, visibleOptions, emptiable, nonEmptiable, ListboxConfig, MsgConfig, Selected, State, Token, closed, focussed, init, input, inputted, isOpen, menuToken, opened, option, options, selected)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Browser.Dom
import Html exposing (Html)
import Html.Keyed
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import List.Extra as List
 
import Menus.Internal.Focus
import Menus.Internal.KeyEvent
import Menus.Focus
import Menus.Select
import Menus.Active
import Menus.Combobox.Internal.Config
import Task


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Focussed value =
    Menus.Internal.Focus.Focussed value

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Selected value =
    Menus.Select.Selected value

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Config options option value selected =
    Menus.Combobox.Internal.Config.Config options option value selected

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Emptiable options option value selected =
    Menus.Combobox.Internal.Config.EmptiableConfig options option value selected

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias NonEmptiable options option value selected =
    Menus.Combobox.Internal.Config.NonEmptiableConfig options option value selected

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Inputted
    = InputChanged String

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
emptiable : Menus.Combobox.Internal.Config.EmptiableConfig options option value selected -> Menus.Combobox.Internal.Config.Config options option value selected
emptiable config =
    Menus.Combobox.Internal.Config.Emptiable config

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
nonEmptiable : Menus.Combobox.Internal.Config.NonEmptiableConfig options option value selected -> Menus.Combobox.Internal.Config.Config options option value selected
nonEmptiable config =
    Menus.Combobox.Internal.Config.NonEmptiable config

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias OpenState value =
    { input : String
    , lastMatch : Maybe value
    , focus : Menus.Focus.Focus value
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type State value
    = Opened (OpenState value)
    | Closed

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
init : State value
init =
    Closed

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
focusOnControl : Menus.Combobox.Internal.Config.Config options option value selected -> MsgConfig value msg -> Cmd msg
focusOnControl config msgConfig =
    Browser.Dom.focus (ids.control (Menus.Combobox.Internal.Config.id config))
        |> Task.attempt (always msgConfig.onNoOp)

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
closed : { config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg } -> ( State value, Cmd msg )
closed args =
    ( Closed, Cmd.none )

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
opened : { state : State value, config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg, selected : selected } -> ( State value, Cmd msg )
opened args =
    case args.state of
        Opened openState ->
            ( Opened openState, Cmd.none )

        Closed ->
            ( Opened
                { input =
                    Menus.Combobox.Internal.Config.selectionToLabel args.config args.selected
                        -- Just option_ ->
                        --     args.config.optionToLabel option_

                        -- Nothing ->
                        --     ""
                , lastMatch =
                    case Menus.Combobox.Internal.Config.selectionToOption args.config args.selected of
                        Just option_ ->
                            Just (Menus.Combobox.Internal.Config.optionToValue args.config option_)

                        Nothing ->
                            Nothing
                , focus =
                    case Menus.Combobox.Internal.Config.selectionToOption args.config args.selected of
                        Just option_ ->
                            Menus.Focus.On (Menus.Combobox.Internal.Config.optionToValue args.config option_)

                        Nothing ->
                            Menus.Focus.Lost
                }
            , focusOnControl args.config args.msgConfig
            )

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
visibleOptions : { state : State value, selected : selected, config : Menus.Combobox.Internal.Config.Config options option value selected, options : options } -> Maybe options
visibleOptions args =
    case args.state of
        Opened openState ->
            case args.config of
                Menus.Combobox.Internal.Config.Emptiable emptiableConfig ->
                    emptiableConfig.visibleOptions openState.input args.selected args.options
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault emptiableConfig.empty
                        |> Just

                Menus.Combobox.Internal.Config.NonEmptiable nonEmptiableConfig ->
                    Just (Tuple.second (nonEmptiableConfig.visibleOptions openState.input args.selected args.options))

        Closed ->
            Nothing

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
focussed : { msg : Menus.Internal.Focus.Focussed value, state : State value, config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( State value, Cmd msg )
focussed args =
    let
        config : Menus.Internal.Focus.Config (State value) value
        config =
            { focusChange =
                \direction ->
                    case args.state of
                        Opened openState ->
                            let
                                visibleOptions_ : options
                                visibleOptions_ =
                                    case args.config of
                                        Menus.Combobox.Internal.Config.Emptiable emptiableConfig ->
                                            emptiableConfig.visibleOptions openState.input args.selected args.options
                                                |> Maybe.map Tuple.second
                                                |> Maybe.withDefault emptiableConfig.empty

                                        Menus.Combobox.Internal.Config.NonEmptiable nonEmptiableConfig ->
                                            Tuple.second (nonEmptiableConfig.visibleOptions openState.input args.selected args.options)
                            in
                            case Menus.Combobox.Internal.Config.focusChange args.config direction openState.focus visibleOptions_ of
                                Menus.Focus.On opt ->
                                    Menus.Focus.On (Menus.Combobox.Internal.Config.optionToValue args.config opt)

                                Menus.Focus.Lost ->
                                    Menus.Focus.Lost

                        Closed ->
                            Menus.Focus.Lost
                    
            , updateFocus =
                \newFocus ->
                    case args.state of
                        Opened openState ->
                            case newFocus of
                                Menus.Focus.On value ->
                                    Opened { openState | lastMatch = Just value, focus = Menus.Focus.On value }

                                Menus.Focus.Lost ->
                                    Opened { openState | focus = Menus.Focus.Lost }

                        Closed ->
                            Closed
            , valueToId = \value -> ids.option (Menus.Combobox.Internal.Config.valueToString args.config value) (Menus.Combobox.Internal.Config.id args.config)
            , optionContainerId = ids.options (Menus.Combobox.Internal.Config.id args.config)
            }
    in
    Menus.Internal.Focus.focussed args.msg args.state args.msgConfig config

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
selected : { msg : Selected value, state : State value, config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
selected args =
    let
        config : Menus.Select.Config selected value
        config =
            { selectChange = \action -> Menus.Combobox.Internal.Config.selectChange args.config action args.selected args.options
            , selectValue = \value -> Menus.Combobox.Internal.Config.selectValue args.config value args.selected args.options
            , currentlyFocussed =
                case args.state of
                    Opened openState ->
                        openState.focus

                    Closed ->
                        Menus.Focus.Lost
            }

        makeChange : Maybe selected -> State value -> Cmd msg -> ( selected, State value, Cmd msg )
        makeChange maybeSelection state cmd =
            case maybeSelection of
                Just selection ->
                    ( selection, Closed, focusOnControl args.config args.msgConfig )

                Nothing ->
                    ( args.selected, state, Cmd.none )

    in
    case args.msg of
        Menus.Select.SelectedSpecific value ->
            makeChange (Menus.Select.selected args.msg config) args.state Cmd.none

        Menus.Select.SelectedFocussed ->
            makeChange (Menus.Select.selected args.msg config) args.state Cmd.none

        Menus.Select.SelectChanged Menus.Select.Cleared ->
            case args.state of
                Opened openState ->
                    case openState.input of
                        "" ->
                            makeChange (Menus.Select.selected args.msg config) args.state Cmd.none

                        _ ->
                            makeChange Nothing (Opened { openState | input = String.dropRight 1 openState.input }) Cmd.none

                Closed ->
                    case Debug.log "sleected" <| Menus.Select.selected args.msg config of
                        Just selection ->
                            makeChange (Just selection) args.state Cmd.none

                        Nothing ->
                            let
                                ( newState, cmd ) =
                                    opened { state = args.state, config = args.config, msgConfig = args.msgConfig, selected = args.selected }

                            in
                            case newState of
                                Opened openState ->
                                    makeChange Nothing (Opened { openState | input = String.dropRight 1 openState.input }) cmd

                                Closed ->
                                    makeChange Nothing Closed Cmd.none
                    -- makeChange (Menus.Select.selected args.msg config) args.state

        Menus.Select.SelectChanged action ->
            makeChange (Menus.Select.selected args.msg config) args.state Cmd.none
    
{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
inputted : { msg : Inputted, state : State value, config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
inputted args =
    case args.msg of
        InputChanged str ->
            let
                visibleOptions_ : Maybe ( Menus.Active.Active option, options )
                visibleOptions_ =
                    case args.config of
                        Menus.Combobox.Internal.Config.Emptiable emptiableConfig ->
                            emptiableConfig.visibleOptions str args.selected args.options

                        Menus.Combobox.Internal.Config.NonEmptiable nonEmptiableConfig ->
                            Just (nonEmptiableConfig.visibleOptions str args.selected args.options)

            in
            case Debug.log "active" <| Maybe.map Tuple.first visibleOptions_ of
                Just (Menus.Active.FocussedAndSelected match) ->
                    ( Menus.Combobox.Internal.Config.selectValue args.config (Menus.Combobox.Internal.Config.optionToValue args.config match) args.selected args.options
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (Menus.Combobox.Internal.Config.optionToValue args.config match)
                        , focus = Menus.Focus.On (Menus.Combobox.Internal.Config.optionToValue args.config match)
                        }
                    , Cmd.none
                    )

                Just (Menus.Active.Focussed match) ->
                    ( args.selected
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (Menus.Combobox.Internal.Config.optionToValue args.config match)
                        , focus = Menus.Focus.On (Menus.Combobox.Internal.Config.optionToValue args.config match)
                        }
                    , Cmd.none
                    )

                Nothing ->
                    case args.state of
                        Opened openState ->
                            ( args.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = openState.lastMatch
                                , focus = Menus.Focus.Lost
                                }
                            , Cmd.none
                            )

                        Closed ->
                            ( args.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = Nothing
                                , focus = Menus.Focus.Lost
                                }
                            , Cmd.none
                            )

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias ListboxConfig option value =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> value
    , valueToString : value -> String
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias MsgConfig value msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Menus.Internal.Focus.Focussed value -> msg
    , onSelected : Selected value -> msg
    , onInput : Inputted -> msg
    , onNoOp : msg
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
ids : { control : String -> String, options : String -> String, option : String -> String -> String }
ids =
    { control = \id -> id ++ "-control"
    , options = \id -> id ++ "-options"
    , option = \id str -> id ++ "-option-" ++ str
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
isOpen : State value -> Bool
isOpen state =
    case state of
        Opened _ ->
            True

        Closed ->
            False



-- Views --

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Token options option value selected msg =
    { state : State value
    , config : Menus.Combobox.Internal.Config.Config options option value selected
    , msgConfig : MsgConfig value msg
    , selected : selected
    , focussed : Maybe value
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
menuToken : { state : State value, config : Menus.Combobox.Internal.Config.Config options option value selected, msgConfig : MsgConfig value msg, selected : selected } -> Token options option value selected msg
menuToken args =
    { state = args.state
    , config = args.config
    , msgConfig = args.msgConfig
    , selected = args.selected
    , focussed =
        case args.state of
            Opened openState ->
                Menus.Internal.Focus.toMaybe openState.focus

            Closed ->
                Nothing
    }

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
input : Token options option value selected msg -> { placeholder : String } -> List (Html.Attribute msg) -> Html msg
input token args attributes =
    case token.state of
        Opened openState ->
            let
                activeDescendant : String
                activeDescendant =
                    Maybe.map (Menus.Combobox.Internal.Config.valueToString token.config) token.focussed
                        |> Maybe.map (ids.option (Menus.Combobox.Internal.Config.id token.config))
                        |> Maybe.withDefault ""
            in
            Html.input
                (List.append attributes
                    [ Attr.id (ids.control (Menus.Combobox.Internal.Config.id token.config))
                    , Aria.controls (ids.options (Menus.Combobox.Internal.Config.id token.config))
                    , Attr.type_ "text"
                    , Attr.autocomplete False
                    , Attr.spellcheck False
                    , Attr.placeholder args.placeholder
                    , Attr.value openState.input
                    , Widget.hasListBoxPopUp
                    , Aria.activeDescendant activeDescendant
                    , Widget.expanded True
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onInput (token.msgConfig.onInput << InputChanged)
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected Menus.Select.SelectedFocussed)
                        , Menus.Internal.KeyEvent.backspace (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.Cleared))
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        ]
                    ]
                )
                []

        Closed ->
            let
                value : String
                value =
                    Menus.Combobox.Internal.Config.selectionToLabel token.config token.selected
                        -- |> Maybe.withDefault ""
                        -- Just opt ->
                        --     token.config.optionToLabel opt

                        -- Nothing ->
                        --     ""
            in
            Html.input
                (List.append attributes
                    [ Attr.id (ids.control (Menus.Combobox.Internal.Config.id token.config))
                    , Aria.controls (ids.options (Menus.Combobox.Internal.Config.id token.config))
                    , Attr.type_ "text"
                    , Attr.autocomplete False
                    , Attr.spellcheck False
                    , Attr.placeholder args.placeholder
                    , Attr.value value
                    , Widget.hasListBoxPopUp
                    , Widget.expanded False
                    , Events.onInput (token.msgConfig.onInput << InputChanged)
                    , Events.onClick token.msgConfig.onOpened
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.MovedLeft))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.MovedRight))
                        , Menus.Internal.KeyEvent.backspace (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.Cleared))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    ]
                )
                []

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
options : Token options option value selected msg -> List (Html.Attribute msg) -> (List ( String, Html msg ) -> Html msg)
options token attributes =
    Html.Keyed.ul
        (List.append attributes
            [ Attr.id (ids.options (Menus.Combobox.Internal.Config.id token.config))
            , Role.listBox
            , Key.tabbable False
            , Menus.Internal.Focus.loseOnMouseLeave token.msgConfig
            ]
        )

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
option : Token options option value selected msg -> { value : value, isSelected : Bool } -> List (Html.Attribute msg) -> (List (Html Never) -> ( String, Html msg ))
option token args attributes children =
    ( ids.option (Menus.Combobox.Internal.Config.valueToString token.config args.value) (Menus.Combobox.Internal.Config.id token.config)
    , List.map (Html.map never) children
        |> Html.li
            (List.append attributes
                [ Attr.id (ids.option (Menus.Combobox.Internal.Config.valueToString token.config args.value) (Menus.Combobox.Internal.Config.id token.config))
                , Role.option
                , Widget.selected args.isSelected
                , Key.tabbable False
                , Menus.Internal.Focus.focusOnMouseMove token.msgConfig token.focussed args.value
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Events.onClick (token.msgConfig.onSelected (Menus.Select.SelectedSpecific args.value))
                ]
            )
    )
