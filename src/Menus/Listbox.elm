module Menus.Listbox exposing (Config, Focussed, Inputted, MsgConfig, Options, Selected, State, Token, button, closed, focussed, inputted, isOpen, menuToken, opened, option, options, selected, ziplist)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Menus.Internal.Base
import Menus.Internal.Focus
import Menus.Internal.KeyEvent
import Menus.Internal.Select
import ZipList



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


type alias Options options =
    ZipList.ZipList options


type alias Config options option value selected =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> options -> value
    , valueToString : value -> String
    , selectionToOption : selected -> Maybe option
    , selectChange : Menus.Internal.Base.Direction -> selected -> options -> selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Internal.Base.Direction -> Maybe value -> options -> Maybe value
    , focusMatch : String -> options -> Maybe value
    }


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


ziplist : { id : String, optionToLabel : option -> String } -> Config (Options option) option Int (Options option)
ziplist config =
    { id = config.id
    , optionToLabel = config.optionToLabel
    , optionToValue =
        \opt opts ->
            ZipList.toList opts
                |> findIndex ((==) opt)
                |> Maybe.withDefault 0
    , valueToString = String.fromInt
    , selectionToOption = Just << ZipList.current
    , selectChange =
        \direction _ opts ->
            case direction of
                Menus.Internal.Base.Left ->
                    ZipList.backward opts

                Menus.Internal.Base.Right ->
                    ZipList.forward opts

                Menus.Internal.Base.Up ->
                    opts

                Menus.Internal.Base.Down ->
                    opts
    , selectValue =
        \idx _ opts ->
            ZipList.goToIndex idx opts
                |> Maybe.withDefault opts
    , focusChange =
        \direction maybePreviousIdx opts ->
            case direction of
                Menus.Internal.Base.Up ->
                    case maybePreviousIdx of
                        Just previousIdx ->
                            Just (max (previousIdx - 1) 0)

                        Nothing ->
                            Just (ZipList.length opts - 1)

                Menus.Internal.Base.Down ->
                    case maybePreviousIdx of
                        Just previousIdx ->
                            Just (min (previousIdx + 1) (ZipList.length opts - 1))

                        Nothing ->
                            Just 0

                Menus.Internal.Base.Left ->
                    Nothing

                Menus.Internal.Base.Right ->
                    Nothing
    , focusMatch =
        \str opts ->
            ZipList.toList opts
                |> findIndex (String.startsWith str << config.optionToLabel)
    }


type Inputted
    = InputAddedTo Char
    | InputCleared


type State value
    = Opened String (Menus.Internal.Focus.Focus value)
    | Closed


closed : State value
closed =
    Closed


opened : State value -> value -> State value
opened state default =
    case state of
        Opened search value ->
            Opened search value

        Closed ->
            Opened "" (Menus.Internal.Focus.HasFocus default)


focussed : { msg : Menus.Internal.Focus.Focussed value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
focussed args =
    Menus.Internal.Focus.focussed args.msg
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
                        Closed
        , valueToId =
            \value ->
                ids.option args.config.id (args.config.valueToString value)
        , optionContainerId = args.config.id
        }


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
            ( selection, Closed, Cmd.none )

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
                        Just newFocus ->
                            ( Opened newSearch (Menus.Internal.Focus.HasFocus newFocus), Cmd.none )

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
            Menus.Internal.Focus.toMaybe focus

        Closed ->
            Nothing


type alias MsgConfig value msg =
    { onOpened : msg
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


isOpen : State value -> Bool
isOpen state =
    state /= Closed



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
                Menus.Internal.Focus.toMaybe focus

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
                    , Widget.hasListBoxPopUp
                    , Widget.expanded True
                    , Aria.controls token.config.id
                    , Aria.activeDescendant activeDescendant
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onClick token.msgConfig.onClosed
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected Menus.Internal.Select.SelectedFocussed)
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        , Menus.Internal.KeyEvent.charKey (token.msgConfig.onInputted << InputAddedTo)
                        , Menus.Internal.KeyEvent.backspace (token.msgConfig.onInputted InputCleared)
                        ]
                    ]
                )

        Closed ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Widget.hasListBoxPopUp
                    , Widget.expanded False
                    , Events.onClick token.msgConfig.onOpened
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Left))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Right))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
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
            , Menus.Internal.Focus.loseOnMouseLeave token.msgConfig
            ]
        )


option : Token options option value selected msg -> { value : value, isSelected : Bool } -> List (Html.Attribute msg) -> (List (Html Never) -> Html msg)
option token args attributes =
    List.map (Html.map never)
        >> Html.li
            (List.append attributes
                [ Attr.id (ids.option token.config.id (token.config.valueToString args.value))
                , Role.option
                , Widget.selected args.isSelected
                , Key.tabbable False
                , Menus.Internal.Focus.focusOnMouseMove token.msgConfig token.focussed args.value
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Events.onClick (token.msgConfig.onSelected (Menus.Internal.Select.SelectedSpecific args.value))
                ]
            )
