module Menus.Listbox exposing (Options, Config, MsgConfig, Focussed, Selected, State, button, closed, currentlyFocussed, focusOn, isOpen, opened, option, options, subscriptions, focussed, selected, ziplist)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Task
import Menus.Internal.Base
import Menus.Internal.KeyEvent
import Menus.Internal.Focus
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

type alias Focussed value = Menus.Internal.Focus.Focussed value
type alias Selected value = Menus.Internal.Select.Selected value


type alias Options options = ZipList.ZipList options


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
    , optionToValue = \opt opts ->
        ZipList.toList opts
            |> findIndex ((==) opt)
            |> Maybe.withDefault 0
    , valueToString = String.fromInt
    , selectionToOption = Just << ZipList.current
    , selectChange = \direction _ opts ->
        case direction of
            Menus.Internal.Base.Left ->
                ZipList.backward opts

            Menus.Internal.Base.Right ->
                ZipList.forward opts

            Menus.Internal.Base.Up ->
                opts

            Menus.Internal.Base.Down ->
                opts
    , selectValue = \idx _ opts ->
        ZipList.goToIndex idx opts
            |> Maybe.withDefault opts
    , focusChange = \direction maybePreviousIdx opts ->
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
    , focusMatch = \str opts ->
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
    Menus.Internal.Focus.focussed args.msg args.state args.msgConfig
        { focusChange = \direction ->
            args.config.focusChange direction (currentlyFocussed args.state) args.options
        , updateFocus = \newFocus ->
            case args.state of
                Opened search _ ->
                    Opened search newFocus

                Closed ->
                    Closed
        , valueToId = \value ->
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


ids =
    { option = \id str -> id ++ "-option-" ++ str
    , control = \id -> id ++ "-control"
    , label = \id -> id ++ "-label"
    }


focusOn : State value -> Maybe value -> State value
focusOn state focus =
    case state of
        Opened search _ ->
            Opened search (Menus.Internal.Focus.fromMaybe focus)

        Closed ->
            Closed


subscriptions : State value -> MsgConfig value msg -> Sub msg
subscriptions state msgConfig =
    case state of
        Opened _ _ ->
            Browser.Events.onMouseDown (Json.Decode.succeed msgConfig.onClosed)

        Closed ->
            Sub.none


isOpen : State value -> Bool
isOpen state =
    state /= Closed



-- Views --


type alias ButtonStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


button : State value -> Config options option value selected -> MsgConfig value msg -> ButtonStyle -> (List (Html msg) -> Html msg)
button state config msgConfig style =
    case state of
        Opened _ _ ->
            Html.button
                [ Attr.id (ids.control config.id)
                , Attr.type_ "button"
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasListBoxPopUp
                , Widget.expanded True
                , Aria.controls config.id
                --, Aria.activeDescendant to-do
                , Menus.Internal.KeyEvent.onKeyDown
                    [ Menus.Internal.KeyEvent.tabWith msgConfig.onClosed { stopPropagation = False, preventDefault = False }
                    , Menus.Internal.KeyEvent.escape msgConfig.onClosed
                    , Menus.Internal.KeyEvent.enter (msgConfig.onSelected Menus.Internal.Select.SelectedFocussed)
                    , Menus.Internal.Focus.keyEvents msgConfig
                    ]
                ]

        Closed ->
            Html.button
                [ Attr.id (ids.control config.id)
                , Attr.type_ "button"
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasListBoxPopUp
                , Widget.expanded False
                , Events.onClick msgConfig.onOpened
                , Menus.Internal.KeyEvent.onKeyDown
                    [ Menus.Internal.KeyEvent.left (msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Left))
                    , Menus.Internal.KeyEvent.right (msgConfig.onSelected (Menus.Internal.Select.SelectChanged Menus.Internal.Base.Right))
                    , Menus.Internal.KeyEvent.down msgConfig.onOpened
                    , Menus.Internal.KeyEvent.up msgConfig.onOpened
                    ]
                ]


type alias OptionsStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


options : State value -> Config options option value selected -> MsgConfig value msg -> Maybe value -> OptionsStyle -> (List (Html msg) -> Html msg)
options state config msgConfig maybeSelected style =
    Html.ul
        [ Attr.id config.id
        , Role.listBox
        , Aria.labelledBy (ids.label config.id)
        , Attr.class "z-10"
        , Attr.class style.classes
        , Attr.classList style.classList
        , Key.tabbable False
            --, Menus.Internal.KeyEvent.charKey (msgConfig.onInput << InputChanged << (++) inputStr_)
            --, Menus.Internal.KeyEvent.backspace (msgConfig.onInput (InputChanged (String.dropRight 1 inputStr_)))
        , Menus.Internal.Focus.loseOnMouseLeave msgConfig
        ]


type alias OptionStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


option : Config options option value selected -> MsgConfig value msg -> value -> Bool -> OptionStyle -> (List (Html Never) -> Html msg)
option config msgConfig value isSelected style =
    List.map (Html.map never)
        >> Html.li
            [ Attr.id (ids.option config.id (config.valueToString value))
            , Role.option
            , Widget.selected isSelected
            , Attr.class style.classes
            , Attr.classList style.classList
            , Key.tabbable False
            , Menus.Internal.Focus.focusOnMouseMove msgConfig value
            , Events.onClick (msgConfig.onSelected (Menus.Internal.Select.SelectedSpecific value))
            ]