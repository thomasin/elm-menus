module Menus.Menu exposing (Config, MsgConfig, Focussed(..), State, button, changeFocus, closed, currentlyFocussed, focusOn, isOpen, onClose, onOpen, opened, li, ul, subscriptions)

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


type State value
    = Opened (Focus value)
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
        Opened value ->
            Opened value

        Closed ->
            Opened (HasFocus default)


currentlyFocussed : State value -> Maybe value
currentlyFocussed state =
    case state of
        Opened (HasFocus value) ->
            Just value

        Opened NoFocus ->
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
    , onNoOp : msg
    }


ids =
    { control = \id -> id ++ "-control"
    , label = \id -> id ++ "-label"
    , li = \id value -> id ++ "-" ++ value ++ "-li"
    }


focusOn : State value -> Maybe value -> State value
focusOn state focus =
    case state of
        Opened _ ->
            Maybe.map (Opened << HasFocus) focus
                |> Maybe.withDefault (Opened NoFocus)

        Closed ->
            Closed


changeFocus : State value -> value -> value -> State value
changeFocus state focus default =
    case state of
        Opened (HasFocus _) ->
            Opened (HasFocus focus)

        Opened NoFocus ->
            Opened (HasFocus default)

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
        Opened _ ->
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
        Opened _ ->
            Html.button
                [ Attr.id (ids.control config.id)
                , Attr.type_ "button"
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasMenuPopUp
                , Widget.expanded True
                , Aria.controls config.id
                ]

        Closed ->
            Html.button
                [ Attr.id (ids.control config.id)
                , Attr.type_ "button"
                , Attr.class style.classes
                , Attr.classList style.classList
                , Widget.hasMenuPopUp
                , Widget.expanded False
                , Events.onClick msgConfig.onOpened
                , Menus.Internal.KeyEvent.onKeyDown
                    [ Menus.Internal.KeyEvent.down msgConfig.onOpened
                    , Menus.Internal.KeyEvent.up msgConfig.onOpened
                    ]
                ]


type alias OptionsStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


ul : State value -> Config option value -> MsgConfig value msg -> OptionsStyle -> (List (Html msg) -> Html msg)
ul state config msgConfig style =
    Html.ul
        [ Attr.id config.id
        , Role.menu
        , Aria.labelledBy (ids.label config.id)
        , Attr.class "z-10"
        , Attr.class style.classes
        , Attr.classList style.classList
        , Key.tabbable (isOpen state)
        , Menus.Internal.KeyEvent.onKeyDown
            [ Menus.Internal.KeyEvent.escape msgConfig.onClosed
            , Menus.Internal.KeyEvent.up (msgConfig.onFocussed FocussedPrevious)
            , Menus.Internal.KeyEvent.down (msgConfig.onFocussed FocussedNext)
            ]
        , Events.onMouseLeave (msgConfig.onFocussed FocusLost)
        ]


type alias OptionStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


li : Config option value -> MsgConfig value msg -> value -> OptionStyle -> (List (Html Never) -> Html msg)
li config msgConfig value style =
    List.map (Html.map never)
        >> Html.li
            [ Attr.id (ids.li config.id (config.valueToString value))
            , Role.menuItem
            , Attr.class style.classes
            , Attr.classList style.classList
            , Key.tabbable False
            , Events.onMouseOver (msgConfig.onFocussed (FocussedSpecific value))
            ]
