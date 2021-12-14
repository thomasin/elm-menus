module Menus.Listbox exposing (Config, MsgConfig, Focussed(..), Selected(..), State, button, changeFocus, closed, currentlyFocussed, focusOn, isOpen, onClose, onOpen, opened, option, options, subscriptions)

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
import Menus.Internal.KeyEvent
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


type alias Options options = ZipList.ZipList options


selectPrevious : Options options -> Options options
selectPrevious opts =
    ZipList.backward opts


selectNext : Options options -> Options options
selectNext opts =
    ZipList.forward opts


selectIndex : Int -> Options options -> Options options
selectIndex idx opts =
    ZipList.goToIndex idx opts
        |> Maybe.withDefault opts


previousIndex : Int -> Options options -> Int
previousIndex idx _ =
    max (idx - 1) 0


nextIndex : Int -> Options options -> Int
nextIndex idx opts =
    min (idx + 1) (ZipList.length opts - 1)


type Focussed value
    = FocussedPrevious
    | FocussedNext
    | FocussedSpecific value
    | FocusLost


type Inputted
    = InputAddedTo Char
    | InputCleared


type Selected value
    = SelectedPrevious
    | SelectedNext
    | SelectedSpecific value
    | SelectedFocussed


type State value
    = Opened String (Focus value)
    | Closed


type Focus value
    = HasFocus value
    | NoFocus


closed : State value
closed =
    Closed


opened : State value -> value -> State value
opened state default =
    case state of
        Opened search value ->
            Opened search value

        Closed ->
            Opened "" (HasFocus default)


currentlyFocussed : State value -> Maybe value
currentlyFocussed state =
    case state of
        Opened _ (HasFocus value) ->
            Just value

        Opened _ NoFocus ->
            Nothing

        Closed ->
            Nothing


type alias Config option value =
    { id : String
    , toLabel : option -> String
    , valueToString : value -> String
    }



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
            Maybe.map (Opened search << HasFocus) focus
                |> Maybe.withDefault (Opened search NoFocus)

        Closed ->
            Closed


changeFocus : State value -> value -> value -> State value
changeFocus state focus default =
    case state of
        Opened search (HasFocus _) ->
            Opened search (HasFocus focus)

        Opened search NoFocus ->
            Opened search (HasFocus default)

        Closed ->
            Closed



{- Focus on given ID.
   This focus is not necessarily a deal breaker for the user, so
   we don't want any actions to be blocked by it - we do nothing no matter
   whether it succeeds or not.
-}


onFocus : String -> MsgConfig value msg -> Cmd msg
onFocus id msgConfig =
    Task.attempt (always msgConfig.onNoOp) (Browser.Dom.focus id)


onOpen : Config option value -> MsgConfig value msg -> Cmd msg
onOpen config msgConfig =
    onFocus config.id msgConfig


onClose : Config option value -> MsgConfig value msg -> Cmd msg
onClose config msgConfig =
    onFocus (ids.control config.id) msgConfig


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


button : State value -> Config option value -> MsgConfig value msg -> ButtonStyle -> (List (Html msg) -> Html msg)
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
                    [ Menus.Internal.KeyEvent.left (msgConfig.onSelected SelectedPrevious)
                    , Menus.Internal.KeyEvent.right (msgConfig.onSelected SelectedNext)
                    , Menus.Internal.KeyEvent.down msgConfig.onOpened
                    , Menus.Internal.KeyEvent.up msgConfig.onOpened
                    ]
                ]


type alias OptionsStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


options : State value -> Config option value -> MsgConfig value msg -> Maybe value -> OptionsStyle -> (List (Html msg) -> Html msg)
options state config msgConfig maybeSelected style =
    Html.ul
        [ Attr.id config.id
        , Role.listBox
        , Aria.labelledBy (ids.label config.id)
        , Aria.activeDescendant (Maybe.withDefault "" (Maybe.map (ids.option config.id << config.valueToString) maybeSelected))
        , Attr.class "z-10"
        , Attr.class style.classes
        , Attr.classList style.classList
        , Key.tabbable (isOpen state)
        , Menus.Internal.KeyEvent.onKeyDown
            [ Menus.Internal.KeyEvent.escape msgConfig.onClosed
            , Menus.Internal.KeyEvent.up (msgConfig.onFocussed FocussedPrevious)
            , Menus.Internal.KeyEvent.down (msgConfig.onFocussed FocussedNext)
            , Menus.Internal.KeyEvent.enter (msgConfig.onSelected SelectedFocussed)
            --, Menus.Internal.KeyEvent.charKey (msgConfig.onInput << InputChanged << (++) inputStr_)
            --, Menus.Internal.KeyEvent.backspace (msgConfig.onInput (InputChanged (String.dropRight 1 inputStr_)))
            ]
        , Events.onMouseLeave (msgConfig.onFocussed FocusLost)
        ]


type alias OptionStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


option : Config option value -> MsgConfig value msg -> value -> Bool -> OptionStyle -> (List (Html Never) -> Html msg)
option config msgConfig value isSelected style =
    List.map (Html.map never)
        >> Html.li
            [ Attr.id (ids.option config.id (config.valueToString value))
            , Role.option
            , Widget.selected isSelected
            , Attr.class style.classes
            , Attr.classList style.classList
            , Key.tabbable False
            , Events.onMouseOver (msgConfig.onFocussed (FocussedSpecific value))
            , Events.onClick (msgConfig.onSelected (SelectedSpecific value))
            ]
