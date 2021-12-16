module Menus.Menu exposing (Config, Focussed, MsgConfig, State, Token, button, closed, focussed, menuToken, opened, option, options)

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


type alias Focussed =
    Menus.Internal.Focus.Focussed Int


type alias Config options =
    { id : String
    , optionsLength : options -> Int
    }


focussed : { msg : Focussed, state : State, config : Config options, msgConfig : MsgConfig msg, options : options } -> ( State, Cmd msg )
focussed args =
    Menus.Internal.Focus.focussed args.msg
        args.state
        args.msgConfig
        { focusChange =
            \direction ->
                let
                    maybePreviousIdx : Maybe Int
                    maybePreviousIdx =
                        currentlyFocussed args.state
                in
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
        , updateFocus =
            \newFocus ->
                case args.state of
                    Opened _ ->
                        Opened newFocus

                    Closed ->
                        Closed
        , valueToId =
            \idx ->
                ids.option args.config.id (String.fromInt idx)
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


ids : { control : String -> String, options : String -> String, option : String -> String -> String }
ids =
    { control = \id -> id ++ "-control"
    , options = \id -> id ++ "-options"
    , option = \id value -> id ++ "-" ++ value ++ "-option"
    }



{- Focus on given ID.
   This focus is not necessarily a deal breaker for the user, so
   we don't want any actions to be blocked by it - we do nothing no matter
   whether it succeeds or not.
-}
-- Views --


type alias Token options msg =
    { state : State
    , config : Config options
    , msgConfig : MsgConfig msg
    , isOpen : Bool
    , focussed : Maybe Int
    }


menuToken : { state : State, config : Config options, msgConfig : MsgConfig msg } -> Token options msg
menuToken args =
    { state = args.state
    , config = args.config
    , msgConfig = args.msgConfig
    , isOpen = args.state /= Closed
    , focussed =
        case args.state of
            Opened focus ->
                Menus.Internal.Focus.toMaybe focus

            Closed ->
                Nothing
    }


button : Token options msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
button token attributes =
    case token.state of
        Opened _ ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Widget.hasMenuPopUp
                    , Widget.expanded True
                    , Aria.controls token.config.id
                    , Events.onBlur token.msgConfig.onClosed
                    , Events.onClick token.msgConfig.onClosed
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.escape token.msgConfig.onClosed
                        , Menus.Internal.Focus.keyEvents token.msgConfig
                        ]
                    ]
                )

        Closed ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Widget.hasMenuPopUp
                    , Widget.expanded False
                    , Events.onClick token.msgConfig.onOpened
                    , Menus.Internal.KeyEvent.onKeyDown
                        [ Menus.Internal.KeyEvent.down token.msgConfig.onOpened
                        , Menus.Internal.KeyEvent.up token.msgConfig.onOpened
                        ]
                    ]
                )


options : Token options msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
options token attributes =
    Html.ul
        (List.append attributes
            [ Attr.id (ids.options token.config.id)
            , Role.menu
            , Key.tabbable False
            , Menus.Internal.Focus.loseOnMouseLeave token.msgConfig
            ]
        )


option : Token options msg -> { idx : Int } -> List (Html.Attribute msg) -> (List (Html Never) -> Html msg)
option token args attributes =
    List.map (Html.map never)
        >> Html.li
            (List.append attributes
                [ Attr.id (ids.option token.config.id (String.fromInt args.idx))
                , Role.menuItem
                , Key.tabbable False
                , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                , Menus.Internal.Focus.focusOnMouseMove token.msgConfig token.focussed args.idx
                ]
            )
