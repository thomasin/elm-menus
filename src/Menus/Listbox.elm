module Menus.Listbox exposing (Config, Focussed, Inputted, MsgConfig, OpenDirection, Selected, State(..), Token, button, closed, focussed, init, inputted, menuToken, opened, option, options, selected)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Menus.Focus
import Menus.Focus.Internal
import Menus.KeyEvent.Internal
import Menus.Select
import Menus.Select.Internal
import Task



{- A custom <select>, with support for keyboard navigation
   This is an attempt to reimplement the native <select> element with custom
   styling. This a basic first step.

   See https://www.w3.org/TR/wai-aria-1.1/#listbox for information on the role
   and https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox for information on
   managing keyboard navigation.
   For development, https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
   is a good page to use as a reference.
-}


type alias Focussed value =
    Menus.Focus.Internal.Focussed value


type alias Selected value =
    Menus.Select.Internal.Selected value


type alias Config options option value selected =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> options -> value
    , valueToString : value -> String
    , selectionToOption : selected -> Maybe option
    , selectChange : Menus.Select.Internal.SelectAction -> selected -> options -> Menus.Select.Change selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Focus.FocusAction -> Maybe value -> options -> Menus.Focus.Focus value
    , focusMatch : String -> options -> Maybe option
    , lastOption : options -> option
    , firstOption : options -> option
    }


focusOnControl : Config options option value selected -> MsgConfig value msg -> Cmd msg
focusOnControl config msgConfig =
    Browser.Dom.focus (ids.control config.id)
        |> Task.attempt (always msgConfig.onNoOp)


type OpenDirection
    = Top
    | Bottom


type Inputted
    = InputAddedTo Char
    | InputCleared


type State value
    = Opened String (Menus.Focus.Focus value)
    | Closed


init : State value
init =
    Closed


closed : { config : Config options option value selected, msgConfig : MsgConfig value msg } -> ( State value, Cmd msg )
closed _ =
    ( Closed, Cmd.none )


opened : OpenDirection -> { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, selected : selected, options : options } -> ( State value, Cmd msg )
opened openDirection args =
    case args.state of
        Opened search value ->
            ( Opened search value
            , Cmd.none
            )

        Closed ->
            case args.config.selectionToOption args.selected of
                Just opt ->
                    ( Opened "" (Menus.Focus.On (args.config.optionToValue opt args.options))
                    , focusOnControl args.config args.msgConfig
                    )

                Nothing ->
                    case openDirection of
                        Top ->
                            ( Opened "" (Menus.Focus.On (args.config.optionToValue (args.config.firstOption args.options) args.options))
                            , focusOnControl args.config args.msgConfig
                            )

                        Bottom ->
                            ( Opened "" (Menus.Focus.On (args.config.optionToValue (args.config.lastOption args.options) args.options))
                            , focusOnControl args.config args.msgConfig
                            )


focussed : { msg : Focussed value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
focussed args =
    Menus.Focus.Internal.focussed args.msg
        args.state
        args.msgConfig
        { focusChange =
            \direction ->
                args.config.focusChange direction (currentlyFocussed args.state) args.options
        , updateFocus =
            \newFocus ->
                case args.state of
                    Opened search _ ->
                        Opened search newFocus

                    Closed ->
                        case newFocus of
                            Menus.Focus.On _ ->
                                Opened "" newFocus

                            Menus.Focus.Lost ->
                                Closed
        , valueToId =
            \value ->
                ids.option args.config.id (args.config.valueToString value)
        , optionContainerId = ids.options args.config.id
        }


selected : { msg : Selected value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
selected args =
    let
        config : Menus.Select.Internal.Config selected value
        config =
            { selectChange = \action -> args.config.selectChange action args.selected args.options
            , selectValue = \value -> args.config.selectValue value args.selected args.options
            , currentlyFocussed = Menus.Focus.fromMaybe (currentlyFocussed args.state)
            }
    in
    case Menus.Select.Internal.selected args.msg config of
        Just selection ->
            ( selection, Closed, focusOnControl args.config args.msgConfig )

        Nothing ->
            ( args.selected, args.state, Cmd.none )


inputted : { msg : Inputted, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
inputted args =
    case args.msg of
        InputAddedTo char ->
            case args.state of
                Opened search focus ->
                    let
                        newSearch : String
                        newSearch =
                            search ++ String.fromChar char
                    in
                    case args.config.focusMatch newSearch args.options of
                        Just match ->
                            ( Opened newSearch (Menus.Focus.On (args.config.optionToValue match args.options)), Cmd.none )

                        Nothing ->
                            ( Opened newSearch focus, Cmd.none )

                Closed ->
                    ( Closed, Cmd.none )

        InputCleared ->
            case args.state of
                Opened _ focus ->
                    ( Opened "" focus, Cmd.none )

                Closed ->
                    ( Closed, Cmd.none )


currentlyFocussed : State value -> Maybe value
currentlyFocussed state =
    case state of
        Opened _ focus ->
            Menus.Focus.Internal.toMaybe focus

        Closed ->
            Nothing


type alias MsgConfig value msg =
    { onOpened : OpenDirection -> msg
    , onClosed : msg
    , onFocussed : Focussed value -> msg
    , onSelected : Selected value -> msg
    , onInputted : Inputted -> msg
    , onNoOp : msg
    }


ids : { control : String -> String, options : String -> String, option : String -> String -> String }
ids =
    { control = \id -> id ++ "-control"
    , options = \id -> id ++ "-options"
    , option = \id str -> id ++ "-option-" ++ str
    }



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
            Opened _ focus ->
                Menus.Focus.Internal.toMaybe focus

            Closed ->
                Nothing
    }


button : Token options option value selected msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
button token attributes =
    case token.state of
        Opened _ _ ->
            let
                activeDescendant : String
                activeDescendant =
                    Maybe.map token.config.valueToString token.focussed
                        |> Maybe.map (ids.option token.config.id)
                        |> Maybe.withDefault ""
            in
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Aria.hasListBoxPopUp
                    , Aria.expanded True
                    , Aria.controls [ ids.options token.config.id ]
                    , Aria.activeDescendant activeDescendant
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onMouseDown token.msgConfig.onClosed
                    , Key.tabbable True
                    , Menus.KeyEvent.Internal.onKeyDown
                        [ Menus.KeyEvent.Internal.escape token.msgConfig.onClosed
                        , Menus.KeyEvent.Internal.enter (token.msgConfig.onSelected Menus.Select.Internal.SelectedFocussed)
                        , Menus.Focus.Internal.keyEvents token.msgConfig
                        , Menus.KeyEvent.Internal.charKey (token.msgConfig.onInputted << InputAddedTo)
                        , Menus.KeyEvent.Internal.backspace (token.msgConfig.onInputted << always InputCleared)
                        ]
                    ]
                )

        Closed ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Aria.hasListBoxPopUp
                    , Aria.expanded False
                    , Events.onMouseDown (token.msgConfig.onOpened Top)
                    , Key.tabbable True
                    , Menus.KeyEvent.Internal.onKeyDown
                        [ Menus.KeyEvent.Internal.left (token.msgConfig.onSelected (Menus.Select.Internal.SelectChanged Menus.Select.Internal.MovedLeft))
                        , Menus.KeyEvent.Internal.right (token.msgConfig.onSelected (Menus.Select.Internal.SelectChanged Menus.Select.Internal.MovedRight))
                        , Menus.KeyEvent.Internal.down (token.msgConfig.onOpened Top)
                        , Menus.KeyEvent.Internal.up (token.msgConfig.onOpened Bottom)
                        ]
                    ]
                )


options : Token options option value selected msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
options token attributes =
    Html.ul
        (List.append attributes
            [ Attr.id (ids.options token.config.id)
            , Role.listBox
            , Key.tabbable False
            , Menus.Focus.Internal.loseOnMouseLeave token.msgConfig
            ]
        )


option : Token options option value selected msg -> { value : value, isSelected : Bool } -> List (Html.Attribute msg) -> (List (Html Never) -> Html msg)
option token args attributes =
    List.map (Html.map never)
        >> Html.li
            (List.append attributes
                [ Attr.id (ids.option token.config.id (token.config.valueToString args.value))
                , Role.option
                , Aria.selected args.isSelected
                , Key.tabbable False
                , Menus.Focus.Internal.focusOnMouseMove token.msgConfig token.focussed args.value
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Events.onClick (token.msgConfig.onSelected (Menus.Select.Internal.SelectedSpecific args.value))
                ]
            )
