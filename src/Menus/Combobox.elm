module Menus.Combobox exposing (Config, Focussed, Inputted, ListboxConfig, MsgConfig, SearchResult(..), Selected, State, Token, closed, focussed, init, input, inputted, isOpen, listbox, menuToken, opened, option, options, selected)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import List.Extra as List
import Menus.Internal.Base
import Menus.Internal.Focus
import Menus.Internal.KeyEvent
import Menus.Internal.Select
import Task



{- A custom <select>, with support for keyboard navigation
   This is an attempt to reimplement the native <select> element with custom
   styling. This a basic first step, with no support for scrolling, search,
   custom styling, disabled options, lots of things.

   See https://www.w3.org/TR/wai-aria-1.1/#listbox for information on the role
   and https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox for information on
   managing keyboard navigation.
   For development, https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
   is a good page to use as a reference.
-}


type alias Focussed value =
    Menus.Internal.Focus.Focussed value


type alias Selected value =
    Menus.Internal.Select.Selected value


type Inputted
    = InputChanged String


type alias Config options option value selected =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> value
    , valueToString : value -> String
    , selectionToOption : selected -> SearchResult option
    , selectChange : Menus.Internal.Base.Direction -> selected -> options -> selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Internal.Base.Direction -> Maybe value -> options -> SearchResult option
    , focusMatch : String -> options -> SearchResult option
    }


type alias OpenState value =
    { input : String
    , lastMatch : Maybe value
    , focus : Menus.Internal.Focus.Focus value
    }


type State value
    = Opened (OpenState value)
    | Closed


init : State value
init =
    Closed


focusOnControl : Config options option value selected -> MsgConfig value msg -> Cmd msg
focusOnControl config msgConfig =
    Browser.Dom.focus (ids.control config.id)
        |> Task.attempt (always msgConfig.onNoOp)


closed : { config : Config options option value selected, msgConfig : MsgConfig value msg } -> ( State value, Cmd msg )
closed args =
    ( Closed, focusOnControl args.config args.msgConfig )


opened : { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, selected : selected } -> ( State value, Cmd msg )
opened args =
    case args.state of
        Opened openState ->
            ( Opened openState, Cmd.none )

        Closed ->
            ( Opened
                { input =
                    case args.config.selectionToOption args.selected of
                        Found option_ ->
                            args.config.optionToLabel option_

                        NotFound ->
                            ""
                , lastMatch =
                    case args.config.selectionToOption args.selected of
                        Found option_ ->
                            Just (args.config.optionToValue option_)

                        NotFound ->
                            Nothing
                , focus =
                    case args.config.selectionToOption args.selected of
                        Found option_ ->
                            Menus.Internal.Focus.HasFocus (args.config.optionToValue option_)

                        NotFound ->
                            Menus.Internal.Focus.NoFocus
                }
            , focusOnControl args.config args.msgConfig
            )


focussed : { msg : Menus.Internal.Focus.Focussed value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
focussed args =
    let
        config : Menus.Internal.Focus.Config (State value) value
        config =
            { focusChange =
                \direction ->
                    case args.config.focusChange direction (currentlyFocussed args.state) args.options of
                        Found opt ->
                            Just (args.config.optionToValue opt)

                        NotFound ->
                            Nothing
            , updateFocus =
                \newFocus ->
                    case args.state of
                        Opened openState ->
                            case newFocus of
                                Menus.Internal.Focus.HasFocus value ->
                                    Opened { openState | lastMatch = Just value, focus = Menus.Internal.Focus.HasFocus value }

                                Menus.Internal.Focus.NoFocus ->
                                    Opened { openState | focus = Menus.Internal.Focus.NoFocus }

                        Closed ->
                            Closed
            , valueToId = \value -> ids.option (args.config.valueToString value) args.config.id
            , optionContainerId = args.config.id
            }
    in
    Menus.Internal.Focus.focussed args.msg args.state args.msgConfig config


selected : { msg : Selected value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
selected args =
    let
        config : Menus.Internal.Select.Config selected value
        config =
            { selectChange = \direction -> args.config.selectChange direction args.selected args.options
            , selectValue = \value -> args.config.selectValue value args.selected args.options
            , currentlyFocussed = Menus.Internal.Focus.fromMaybe (currentlyFocussed args.state)
            }
    in
    case Menus.Internal.Select.selected args.msg config of
        Just selection ->
            ( selection, Closed, focusOnControl args.config args.msgConfig )

        Nothing ->
            ( args.selected, args.state, Cmd.none )


inputted : { msg : Inputted, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
inputted args =
    case args.msg of
        InputChanged str ->
            case args.config.focusMatch str args.options of
                Found match ->
                    ( args.config.selectValue (args.config.optionToValue match) args.selected args.options
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (args.config.optionToValue match)
                        , focus = Menus.Internal.Focus.HasFocus (args.config.optionToValue match)
                        }
                    , Cmd.none
                    )

                NotFound ->
                    case args.state of
                        Opened openState ->
                            ( args.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = openState.lastMatch
                                , focus = Menus.Internal.Focus.NoFocus
                                }
                            , Cmd.none
                            )

                        Closed ->
                            ( args.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = Nothing
                                , focus = Menus.Internal.Focus.NoFocus
                                }
                            , Cmd.none
                            )


currentlyFocussed : State value -> Maybe value
currentlyFocussed state =
    case state of
        Opened openState ->
            Menus.Internal.Focus.toMaybe openState.focus

        Closed ->
            Nothing


type SearchResult item
    = Found item
    | NotFound


searchResult : Maybe item -> SearchResult item
searchResult maybeItem =
    case maybeItem of
        Just item ->
            Found item

        Nothing ->
            NotFound


type alias ListboxConfig option value =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> value
    , valueToString : value -> String
    }



--visibleOptions str opts =
--    List.filter (String.contains str << config.optionToLabel) opts


listbox : ListboxConfig option value -> Config (List option) option value (Maybe option)
listbox config =
    { id = config.id
    , optionToLabel = config.optionToLabel
    , optionToValue = config.optionToValue
    , valueToString = config.valueToString
    , selectionToOption = searchResult
    , selectChange =
        \direction maybeSelected opts ->
            case direction of
                Menus.Internal.Base.Left ->
                    case maybeSelected of
                        Just selected_ ->
                            case List.splitWhen ((==) selected_) opts of
                                Just ( previous, _ ) ->
                                    List.last previous

                                Nothing ->
                                    List.last opts

                        Nothing ->
                            List.last opts

                Menus.Internal.Base.Right ->
                    case maybeSelected of
                        Just selected_ ->
                            case List.splitWhen ((==) selected_) opts of
                                Just ( _, _ :: os ) ->
                                    List.head os

                                Just ( _, [] ) ->
                                    Nothing

                                Nothing ->
                                    List.head opts

                        Nothing ->
                            List.head opts

                Menus.Internal.Base.Up ->
                    Nothing

                Menus.Internal.Base.Down ->
                    Nothing
    , selectValue =
        \value _ opts ->
            List.filter ((==) value << config.optionToValue) opts
                |> List.head
    , focusChange =
        \direction value opts ->
            case direction of
                Menus.Internal.Base.Up ->
                    case value of
                        Just currentFocus ->
                            case List.splitWhen ((==) currentFocus << config.optionToValue) opts of
                                Just ( previous, _ ) ->
                                    searchResult (List.last previous)

                                Nothing ->
                                    searchResult (List.last opts)

                        Nothing ->
                            searchResult (List.last opts)

                Menus.Internal.Base.Down ->
                    case value of
                        Just currentFocus ->
                            case List.splitWhen ((==) currentFocus << config.optionToValue) opts of
                                Just ( _, _ :: os ) ->
                                    searchResult (List.head os)

                                Just ( _, [] ) ->
                                    searchResult Nothing

                                Nothing ->
                                    searchResult (List.head opts)

                        Nothing ->
                            searchResult (List.head opts)

                Menus.Internal.Base.Left ->
                    NotFound

                Menus.Internal.Base.Right ->
                    NotFound
    , focusMatch =
        \str opts ->
            List.filter (String.startsWith str << config.optionToLabel) opts
                |> List.head
                |> searchResult
    }


type alias MsgConfig value msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Menus.Internal.Focus.Focussed value -> msg
    , onSelected : Selected value -> msg
    , onInput : Inputted -> msg
    , onNoOp : msg
    }


ids : { control : String -> String, options : String -> String, option : String -> String -> String }
ids =
    { control = \id -> id ++ "-control"
    , options = \id -> id ++ "-options"
    , option = \id str -> id ++ "-option-" ++ str
    }


isOpen : State value -> Bool
isOpen state =
    case state of
        Opened _ ->
            True

        Closed ->
            False



-- Views --


type alias Token options option value selected msg =
    { state : State value
    , config : Config options option value selected
    , msgConfig : MsgConfig value msg
    , selected : selected
    , focussed : Maybe value
    }


menuToken : { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, selected : selected } -> Token options option value selected msg
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


input : Token options option value selected msg -> { placeholder : String } -> List (Html.Attribute msg) -> Html msg
input token args attributes =
    case token.state of
        Opened openState ->
            let
                activeDescendant : String
                activeDescendant =
                    Maybe.map token.config.valueToString token.focussed
                        |> Maybe.map (ids.option token.config.id)
                        |> Maybe.withDefault ""
            in
            Html.input
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Aria.controls token.config.id
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
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected Menus.Internal.Select.SelectedFocussed)
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        ]
                    ]
                )
                []

        Closed ->
            let
                value : String
                value =
                    case token.config.selectionToOption token.selected of
                        Found opt ->
                            token.config.optionToLabel opt

                        NotFound ->
                            ""
            in
            Html.input
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Aria.controls token.config.id
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
                        [ Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Left))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Right))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    ]
                )
                []


options : Token options option value selected msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
options token attributes =
    Html.ul
        (List.append attributes
            [ Attr.id (ids.options token.config.id)
            , Role.listBox
            , Key.tabbable False
            , Menus.Internal.Focus.loseOnMouseLeave token.msgConfig
            ]
        )


option : Token options option value selected msg -> { value : value, isSelected : Bool } -> List (Html.Attribute msg) -> (List (Html Never) -> Html msg)
option token args attributes =
    List.map (Html.map never)
        >> Html.li
            (List.append attributes
                [ Attr.id (ids.option (token.config.valueToString args.value) token.config.id)
                , Role.option
                , Widget.selected args.isSelected
                , Key.tabbable False
                , Menus.Internal.Focus.focusOnMouseMove token.msgConfig token.focussed args.value
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Events.onClick (token.msgConfig.onSelected (Menus.Internal.Select.SelectedSpecific args.value))
                ]
            )
