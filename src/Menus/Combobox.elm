module Menus.Combobox exposing (Config, ListboxConfig, listbox, Focussed, Inputted(..), MsgConfig, Selected(..), State, closed, container, currentlyFocussed, focussed, init, input, inputted, isOpen, opened, option, options, selected, subscriptions, inputStr, control, lastMatch, setMatch, TokenGenerator(..), tokenised, Token, SearchResult(..))

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
import List.Extra as List
import Task
import Menus.Internal.Base
import Menus.Internal.KeyEvent
import Menus.Internal.Focus


lastMatch state =
    case state of
        Opened openState ->
            openState.lastMatch

        Closed ->
            Nothing


setMatch : State value -> (Maybe value -> value) -> State value
setMatch state f =
    case state of
        Opened openState ->
            Opened
                { openState
                | lastMatch = Just (f openState.lastMatch)
                , focus = Menus.Internal.Focus.HasFocus (f openState.lastMatch)
                }

        Closed ->
            Closed


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


type Selected value
    = SelectChanged Menus.Internal.Base.Direction
    | SelectedSpecific value
    | SelectedFocussed


type Inputted
    = InputAddedTo String
    | InputChanged String
    | BackspacePressed


type alias OpenState value =
    { input : String
    , lastMatch : Maybe value
    , focus : Menus.Internal.Focus.Focus value
    }


type State value
    = Opened (OpenState value)
    | Closed


init =
    Closed


closed : { config : Config options option value selected, msgConfig : MsgConfig value msg } -> ( State value, Cmd msg )
closed args =
    ( Closed, onClose args.config args.msgConfig )


opened : { state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, selected : selected } -> ( State value, Cmd msg )
opened args =
    case args.state of
        Opened openState ->
            ( Opened openState, onOpen args.config args.msgConfig )

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
            , onOpen args.config args.msgConfig
            )


--focussed : { msg : Focussed value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
--focussed args =
--    case args.msg of
--        FocussedSpecific value ->
--            ( focusOn args.state (Just value)
--            , onFocus (ids.option args.config.id (args.config.valueToString value)) args.msgConfig
--            )

--        FocussedChanged direction ->
--            case args.config.focusChange direction (currentlyFocussed args.state) args.options of
--                Found newFocus ->
--                    ( focusOn args.state (Just (args.config.optionToValue newFocus))
--                    , onFocus (ids.option args.config.id (args.config.valueToString (args.config.optionToValue (newFocus)))) args.msgConfig
--                    )

--                NotFound ->
--                    ( args.state
--                    , Cmd.none
--                    )

--        FocusLost ->
--            ( focusOn args.state Nothing
--            , onFocus "" args.msgConfig
--            )
focussed : { msg : Menus.Internal.Focus.Focussed value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options } -> ( State value, Cmd msg )
focussed args =
    Menus.Internal.Focus.focussed args.msg args.state args.msgConfig
        { focusChange = \direction ->
            case args.config.focusChange direction (currentlyFocussed args.state) args.options of
                Found opt ->
                    Just (args.config.optionToValue opt)

                NotFound ->
                    Nothing
        , updateFocus = \newFocus ->
            case args.state of
                Opened openState ->
                    case newFocus of
                        Menus.Internal.Focus.HasFocus value ->
                            Opened { openState | lastMatch = Just value, focus = Menus.Internal.Focus.HasFocus value }

                        Menus.Internal.Focus.NoFocus ->
                            Opened { openState | focus = Menus.Internal.Focus.NoFocus }

                Closed ->
                    Closed
        , valueToId = \value ->
            ids.option args.config.id (args.config.valueToString value)
        }


selected : { msg : Selected value, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
selected args =
    case args.msg of
        SelectedSpecific value ->
            ( args.config.selectValue value args.selected args.options, Closed, onClose args.config args.msgConfig )

        SelectChanged direction ->
            ( args.config.selectChange direction args.selected args.options 
            , Closed
            , onClose args.config args.msgConfig
            )

        SelectedFocussed ->
            case currentlyFocussed args.state of
                Just focus ->
                    ( args.config.selectValue focus args.selected args.options, Closed, onClose args.config args.msgConfig )

                Nothing ->
                    ( args.selected, args.state, Cmd.none )


inputted : { msg : Inputted, state : State value, config : Config options option value selected, msgConfig : MsgConfig value msg, options : options, selected : selected } -> ( selected, State value, Cmd msg )
inputted args =
    let
        focusOnNewInput : String -> ( selected, State value, Cmd msg )
        focusOnNewInput str =
            case args.config.focusMatch str args.options of
                Found match ->
                    ( args.config.selectValue (args.config.optionToValue match) args.selected args.options
                    , Opened
                        { input = String.trimLeft str
                        , lastMatch = Just (args.config.optionToValue match)
                        , focus = Menus.Internal.Focus.HasFocus (args.config.optionToValue match)
                        }
                    , Menus.Internal.Focus.attemptFocus (ids.input args.config.id) args.msgConfig
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
                            , Menus.Internal.Focus.attemptFocus (ids.input args.config.id) args.msgConfig
                            )

                        Closed ->
                            ( args.selected
                            , Opened
                                { input = String.trimLeft str
                                , lastMatch = Nothing
                                , focus = Menus.Internal.Focus.NoFocus
                                }
                            , Menus.Internal.Focus.attemptFocus (ids.input args.config.id) args.msgConfig
                            )
            
    in
    case args.msg of
        InputChanged str ->
            focusOnNewInput str

        InputAddedTo str ->
            case args.state of
                Opened openState ->
                    focusOnNewInput (openState.input ++ str)

                Closed ->
                    ( args.selected, args.state, Cmd.none )

        BackspacePressed ->
            ( args.selected, args.state, Cmd.none )


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
        Just item -> Found item
        Nothing -> NotFound


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
    , selectChange = \direction maybeSelected opts ->
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
    , selectValue = \value _ opts ->
        List.filter ((==) value << config.optionToValue) opts
            |> List.head
    , focusChange = \direction value opts ->
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
                                searchResult (Nothing)

                            Nothing ->
                                searchResult (List.head opts)

                    Nothing ->
                        searchResult (List.head opts)


            Menus.Internal.Base.Left ->
                NotFound

            Menus.Internal.Base.Right ->
                NotFound
    , focusMatch = \str opts ->
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


ids =
    { option = \id str -> id ++ "-option-" ++ str
    , control = \id -> id ++ "-control"
    , label = \id -> id ++ "-label"
    , input = \id -> id ++ "-input"
    }



{- Focus on given ID.
   This focus is not necessarily a deal breaker for the user, so
   we don't want any actions to be blocked by it - we do nothing no matter
   whether it succeeds or not.
-}


onOpen : Config options option value selected -> MsgConfig value msg -> Cmd msg
onOpen config msgConfig =
    Menus.Internal.Focus.attemptFocus (ids.input config.id) msgConfig


onClose : Config options option value selected -> MsgConfig value msg -> Cmd msg
onClose config msgConfig =
    Menus.Internal.Focus.attemptFocus (ids.input config.id) msgConfig


subscriptions : State value -> MsgConfig value msg -> Sub msg
subscriptions state msgConfig =
    case state of
        Opened _ ->
            Browser.Events.onMouseDown (Json.Decode.succeed msgConfig.onClosed)

        Closed ->
            Sub.none


isOpen : State value -> Bool
isOpen state =
    case state of
        Opened _ ->
            True

        Closed ->
            False



-- Views --


type TokenGenerator options option value selected msg
    = WithToken (Token options option value selected msg)


type Token options option value selected msg = Token
    { state : State value
    , config : Config options option value selected
    , msgConfig : MsgConfig value msg
    , selected : selected
    }


tokenised : ( TokenGenerator options option value selected msg -> element ) -> State value -> Config options option value selected -> MsgConfig value msg -> selected -> ( Token options option value selected msg, element )
tokenised element state config msgConfig selected_ =
    let token = Token { state = state, config = config, msgConfig = msgConfig, selected = selected_ } in
    ( token
    , element (WithToken token)
    )


type alias InputStyle =
    { placeholder : String
    , classes : String
    , classList : List ( String, Bool )
    }


inputStr : State value -> String
inputStr state =
    case state of
        Opened openState ->
            openState.input

        Closed ->
            ""


input : TokenGenerator options option value selected msg -> Maybe String -> InputStyle -> Html msg
input (WithToken token) contentsWhileClosed style =
    input_ token contentsWhileClosed style


input_ : Token options option value selected msg -> Maybe String -> InputStyle -> Html msg
input_ (Token token) contentsWhileClosed style =
    case token.state of
        Opened openState ->
            Html.input
                [ Attr.id (ids.input token.config.id)
                , Aria.controls token.config.id
                , Attr.type_ "text"
                , Attr.autocomplete False
                , Attr.spellcheck False
                , Attr.placeholder style.placeholder
                , Attr.value openState.input
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasListBoxPopUp
                , Widget.expanded True
                , Events.onInput (token.msgConfig.onInput << InputChanged)
                , Menus.Internal.KeyEvent.onKeyDown
                    (if String.isEmpty openState.input then
                        [ Menus.Internal.KeyEvent.backspace (token.msgConfig.onInput BackspacePressed)
                        , Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected SelectedFocussed)
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        ]
                    else
                        [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected SelectedFocussed)
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        ]
                    )
                ]
                []

        Closed ->
            Html.input
                [ Attr.id (ids.input token.config.id)
                , Aria.controls token.config.id
                , Attr.type_ "text"
                , Attr.autocomplete False
                , Attr.spellcheck False
                , Attr.placeholder style.placeholder
                , Attr.value (Maybe.withDefault "" contentsWhileClosed)
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasListBoxPopUp
                , Widget.expanded False
                , Events.onInput (token.msgConfig.onInput << InputChanged)
                , Events.onClick token.msgConfig.onOpened
                , Menus.Internal.KeyEvent.onKeyDown
                    (if String.isEmpty (Maybe.withDefault "" contentsWhileClosed) then
                        [ Menus.Internal.KeyEvent.backspace (token.msgConfig.onInput BackspacePressed)
                        , Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (SelectChanged Menus.Internal.Base.Left))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (SelectChanged Menus.Internal.Base.Right))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    else
                        [ Menus.Internal.KeyEvent.left (token.msgConfig.onSelected (SelectChanged Menus.Internal.Base.Left))
                        , Menus.Internal.KeyEvent.right (token.msgConfig.onSelected (SelectChanged Menus.Internal.Base.Right))
                        , Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    )
                ]
                []


type alias ControlStyle =
    { classes : String
    , classList : List ( String, Bool )
    , placeholder : String
    }


control : TokenGenerator options option value selected msg -> Maybe String -> ControlStyle -> (List (Html msg) -> Html msg)
control (WithToken token) contentsWhileClosed style =
    control_ token contentsWhileClosed style


control_ : Token options option value selected msg -> Maybe String -> ControlStyle -> (List (Html msg) -> Html msg)
control_ (Token token) contentsWhileClosed style =
    case token.state of
        Opened openState ->
            let
                dataValue = if String.isEmpty openState.input then style.placeholder else openState.input
            in
            Html.div
                [ Attr.id (ids.control token.config.id)
                , Attr.attribute "data-value" dataValue
                , Aria.controls token.config.id
                , Attr.class style.classes
                ]

        Closed ->
            let
                dataValue = if String.isEmpty (Maybe.withDefault "" contentsWhileClosed) then style.placeholder else (Maybe.withDefault "" contentsWhileClosed)
            in
            Html.div
                [ Attr.id (ids.control token.config.id)
                , Aria.controls token.config.id
                , Attr.attribute "data-value" dataValue
                , Events.onClick token.msgConfig.onOpened
                , Attr.class style.classes
                ]




type alias ContainerStyle =
    { classes : String
    }


container : MsgConfig value msg -> ContainerStyle -> (List (Html msg) -> Html msg)
container msgConfig style =
    Html.div
        [ Events.stopPropagationOn "mousedown" (Json.Decode.succeed ( msgConfig.onNoOp, True ))
        , Attr.class style.classes
        ]


type alias OptionsStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


options : Token options option value selected msg -> OptionsStyle -> (List (Html msg) -> Html msg)
options (Token token) style =
    let
        inputStr_ = inputStr token.state

        activeDescendant =
            case token.config.selectionToOption token.selected of
                Found option_ ->
                    token.config.optionToValue option_
                        |> token.config.valueToString
                        |> ids.option token.config.id

                NotFound ->
                    ""
    in
    Html.ul
        [ Attr.id token.config.id
        , Role.listBox
        , Aria.labelledBy (ids.label token.config.id)
        , Aria.activeDescendant activeDescendant
        , Attr.class "z-10"
        , Attr.class style.classes
        , Attr.classList style.classList
        , Key.tabbable (isOpen token.state)
        , Menus.Internal.KeyEvent.onKeyDown
            [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
            , Menus.Internal.KeyEvent.enter (token.msgConfig.onSelected SelectedFocussed)
            , Menus.Internal.KeyEvent.charKey (token.msgConfig.onInput << InputAddedTo)
            , Menus.Internal.KeyEvent.backspace (token.msgConfig.onInput (InputChanged (String.dropRight 1 inputStr_)))
            , Menus.Internal.Focus.keyEvents token.msgConfig
            ]
        , Menus.Internal.Focus.loseOnMouseLeave token.msgConfig
        ]


type alias OptionStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


option : Token options option value selected msg -> value -> Bool -> OptionStyle -> (List (Html Never) -> Html msg)
option (Token token) value isSelected style =
    List.map (Html.map never)
        >> Html.li
            [ Attr.id (ids.option token.config.id (token.config.valueToString value))
            , Role.option
            , Widget.selected isSelected
            , Attr.class style.classes
            , Attr.classList style.classList
            , Key.tabbable False
            , Menus.Internal.Focus.focusOnMouseOver token.msgConfig value
            , Events.onClick (token.msgConfig.onSelected (SelectedSpecific value))
            ]

