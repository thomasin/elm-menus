module Menus.Menu exposing (Config, MsgConfig, Focussed, State, button, closed, currentlyFocussed, isOpen, opened, li, ul, subscriptions, focussed)

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
import ZipList


type alias Focussed = Menus.Internal.Focus.Focussed Int


type alias Config options =
    { id : String
    , optionsLength : options -> Int
    }


focussed : { msg : Focussed, state : State, config : Config options, msgConfig : MsgConfig msg, options : options } -> ( State, Cmd msg )
focussed args =
    Menus.Internal.Focus.focussed args.msg args.state args.msgConfig
        { focusChange = \direction ->
            let maybePreviousIdx = currentlyFocussed args.state in
            case direction of
                Menus.Internal.Base.Up ->
                    case maybePreviousIdx of
                        Just previousIdx ->
                            Just (max (previousIdx - 1) 0)

                        Nothing ->
                            Just (args.config.optionsLength args.options - 1)

                Menus.Internal.Base.Down ->
                    case maybePreviousIdx of 
                        Just previousIdx ->
                            Just (min (previousIdx + 1) (args.config.optionsLength args.options - 1))

                        Nothing ->
                            Just 0


                Menus.Internal.Base.Left ->
                    Nothing

                Menus.Internal.Base.Right ->
                    Nothing
        , updateFocus = \newFocus ->
            case args.state of
                Opened _ ->
                    Opened newFocus

                Closed ->
                    Closed
        , valueToId = \idx ->
            ids.li args.config.id (String.fromInt idx)
        , optionContainerId = args.config.id
        }


type State
    = Opened (Menus.Internal.Focus.Focus Int)
    | Closed


closed : State
closed =
    Closed


opened : State -> Int -> State
opened state default =
    case state of
        Opened value ->
            Opened value

        Closed ->
            Opened (Menus.Internal.Focus.HasFocus default)


currentlyFocussed : State -> Maybe Int
currentlyFocussed state =
    case state of
        Opened focus ->
            Menus.Internal.Focus.toMaybe focus

        Closed ->
            Nothing


type alias MsgConfig msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Focussed -> msg
    , onNoOp : msg
    }


ids =
    { control = \id -> id ++ "-control"
    , label = \id -> id ++ "-label"
    , li = \id value -> id ++ "-" ++ value ++ "-li"
    }


focusOn : State -> Maybe Int -> State
focusOn state focus =
    case state of
        Opened _ ->
            Opened (Menus.Internal.Focus.fromMaybe focus)

        Closed ->
            Closed



{- Focus on given ID.
   This focus is not necessarily a deal breaker for the user, so
   we don't want any actions to be blocked by it - we do nothing no matter
   whether it succeeds or not.
-}


subscriptions : State -> MsgConfig msg -> Sub msg
subscriptions state msgConfig =
    case state of
        Opened _ ->
            Browser.Events.onMouseDown (Json.Decode.succeed msgConfig.onClosed)

        Closed ->
            Sub.none


isOpen : State -> Bool
isOpen state =
    state /= Closed



-- Views --


type alias ButtonStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


button : State -> Config options -> MsgConfig msg -> ButtonStyle -> (List (Html msg) -> Html msg)
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
                , Menus.Internal.KeyEvent.onKeyDown
                    [ Menus.Internal.KeyEvent.tabWith msgConfig.onClosed { stopPropagation = False, preventDefault = False }
                    , Menus.Internal.KeyEvent.escape msgConfig.onClosed
                    , Menus.Internal.Focus.keyEvents msgConfig
                    ]
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


ul : State -> Config options -> MsgConfig msg -> OptionsStyle -> (List (Html msg) -> Html msg)
ul state config msgConfig style =
    Html.ul
        [ Attr.id config.id
        , Role.menu
        , Aria.labelledBy (ids.label config.id)
        , Attr.class "z-10"
        , Attr.class style.classes
        , Attr.classList style.classList
        , Key.tabbable False
        , Menus.Internal.Focus.loseOnMouseLeave msgConfig
        ]


type alias OptionStyle =
    { classes : String
    , classList : List ( String, Bool )
    }


li : Config options -> MsgConfig msg -> Int -> OptionStyle -> (List (Html Never) -> Html msg)
li config msgConfig idx style =
    List.map (Html.map never)
        >> Html.li
            [ Attr.id (ids.li config.id (String.fromInt idx))
            , Role.menuItem
            , Attr.class style.classes
            , Attr.classList style.classList
            , Key.tabbable False
            , Menus.Internal.Focus.focusOnMouseMove msgConfig idx
            ]
