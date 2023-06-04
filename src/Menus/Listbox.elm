module Menus.Listbox exposing (Config, Focussed, Inputted, MsgConfig, Options, Selected, State, Token, button, closed, focussed, init, inputted, isOpen, menuToken, opened, option, options, selected, ziplist)

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
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
 
import Menus.Internal.Focus
import Menus.Internal.KeyEvent
import Menus.Select
import Menus.Focus
import Task
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
type alias Options options =
    ZipList.ZipList options


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Config options option value selected =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> options -> value
    , valueToString : value -> String
    , selectionToOption : selected -> Maybe option
    , selectChange : Menus.Select.SelectAction -> selected -> options -> Menus.Select.Change selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Focus.FocusAction -> Maybe value -> options -> Menus.Focus.Focus value
    , focusMatch : String -> options -> List option
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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
        \action _ opts ->
            case action of
                Menus.Select.MovedLeft ->
                    Menus.Select.ChangedTo (ZipList.backward opts)

                Menus.Select.MovedRight ->
                    Menus.Select.ChangedTo (ZipList.forward opts)

                Menus.Select.Cleared ->
                    Menus.Select.NotChanged
    , selectValue =
        \idx _ opts ->
            ZipList.goToIndex idx opts
                |> Maybe.withDefault opts
    , focusChange =
        \action maybePreviousIdx opts ->
            case action of
                Menus.Focus.MovedUp ->
                    case maybePreviousIdx of
                        Just previousIdx ->
                            Menus.Focus.On (max (previousIdx - 1) 0)

                        Nothing ->
                            Menus.Focus.On (ZipList.length opts - 1)

                Menus.Focus.MovedDown ->
                    case maybePreviousIdx of
                        Just previousIdx ->
                            Menus.Focus.On (min (previousIdx + 1) (ZipList.length opts - 1))

                        Nothing ->
                            Menus.Focus.On 0

                Menus.Focus.MovedLeft ->
                    Menus.Focus.Lost

                Menus.Focus.MovedRight ->
                    Menus.Focus.Lost
    , focusMatch =
        \str opts ->
            List.filter (String.startsWith (String.toLower str) << String.toLower << config.optionToLabel) (ZipList.toList opts)
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
focusOnControl : Config options option value selected -> MsgConfig value msg -> Cmd msg
focusOnControl config msgConfig =
    Browser.Dom.focus (ids.control config.id)
        |> Task.attempt (always msgConfig.onNoOp)


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Inputted
    = InputAddedTo Char
    | InputCleared


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type State value
    = Opened String (Menus.Focus.Focus value)
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
closed : { config : Config options option value selected, msgConfig : MsgConfig value msg } -> ( State value, Cmd msg )
closed args =
    -- ( Closed, focusOnControl args.config args.msgConfig )
    ( Closed, Cmd.none )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
opened : { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg } -> value -> ( State value, Cmd msg )
opened args default =
    case args.state of
        Opened search value ->
            ( Opened search value
            , Cmd.none
            )

        Closed ->
            ( Opened "" (Menus.Focus.On default)
            , focusOnControl args.config args.msgConfig
            )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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
        , optionContainerId = ids.options args.config.id
        }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
selected : { msg : Selected value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
selected args =
    let
        config : Menus.Select.Config selected value
        config =
            { selectChange = \action -> args.config.selectChange action args.selected args.options
            , selectValue = \value -> args.config.selectValue value args.selected args.options
            , currentlyFocussed = Menus.Focus.fromMaybe (currentlyFocussed args.state)
            }
    in
    case Menus.Select.selected args.msg config of
        Just selection ->
            ( selection, Closed, focusOnControl args.config args.msgConfig )

        Nothing ->
            ( args.selected, args.state, Cmd.none )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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
                        match :: _ ->
                            ( Opened newSearch (Menus.Focus.On (args.config.optionToValue match args.options)), Cmd.none )

                        [] ->
                            ( Opened newSearch focus, Cmd.none )

                Closed ->
                    ( Closed, Cmd.none )

        InputCleared ->
            case args.state of
                Opened _ focus ->
                    ( Opened "" focus, Cmd.none )

                Closed ->
                    ( Closed, Cmd.none )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
currentlyFocussed : State value -> Maybe value
currentlyFocussed state =
    case state of
        Opened _ focus ->
            Menus.Internal.Focus.toMaybe focus

        Closed ->
            Nothing


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias MsgConfig value msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Focussed value -> msg
    , onSelected : Selected value -> msg
    , onInputted : Inputted -> msg
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
    state /= Closed



-- Views --


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Token options option value selected msg =
    { state : State value
    , config : Config options option value selected
    , msgConfig : MsgConfig value msg
    , selected : selected
    , focussed : Maybe value
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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
                    , Aria.controls (ids.options token.config.id)
                    , Aria.activeDescendant activeDescendant
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onClick token.msgConfig.onClosed
                    , Key.tabbable True
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected Menus.Select.SelectedFocussed)
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
                    , Key.tabbable True
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.MovedLeft))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (Menus.Select.SelectChanged Menus.Select.MovedRight))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    ]
                )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
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
                , Events.onClick (token.msgConfig.onSelected (Menus.Select.SelectedSpecific args.value))
                ]
            )
