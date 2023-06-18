module Menus.Menu exposing (Config, Focussed, Link, MsgConfig, OpenDirection(..), State, Token, button, closed, focussed, init, link, menuToken, opened, options)

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
import Task


type OpenDirection
    = Top
    | Bottom


type alias Focussed =
    Menus.Focus.Internal.Focussed Int


type alias Config options =
    { id : String
    , optionsLength : options -> Int
    }


focussed : { msg : Focussed, state : State, config : Config options, msgConfig : MsgConfig msg, options : options } -> ( State, Cmd msg )
focussed args =
    let
        ( state, cmd ) =
            Menus.Focus.Internal.focussed args.msg
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
                            Menus.Focus.MovedUp ->
                                case maybePreviousIdx of
                                    Just previousIdx ->
                                        Menus.Focus.On (max (previousIdx - 1) 0)

                                    Nothing ->
                                        Menus.Focus.On (args.config.optionsLength args.options - 1)

                            Menus.Focus.MovedDown ->
                                case maybePreviousIdx of
                                    Just previousIdx ->
                                        Menus.Focus.On (min (previousIdx + 1) (args.config.optionsLength args.options - 1))

                                    Nothing ->
                                        Menus.Focus.On 0

                            Menus.Focus.MovedLeft ->
                                Menus.Focus.Lost

                            Menus.Focus.MovedRight ->
                                Menus.Focus.Lost
                , updateFocus =
                    \newFocus ->
                        Opened newFocus
                , valueToId =
                    \idx ->
                        ids.option args.config.id (String.fromInt idx)
                , optionContainerId = ids.options args.config.id
                }
    in
    case state of
        Opened (Menus.Focus.On newFocus) ->
            case args.state of
                Opened (Menus.Focus.On oldFocus) ->
                    if newFocus == oldFocus then
                        ( state, cmd )

                    else
                        ( state, Cmd.batch [ cmd, focusOnItem args.config.id newFocus args.msgConfig.onNoOp ] )

                Opened Menus.Focus.Lost ->
                    ( state, focusOnControl args.config.id args.msgConfig.onNoOp )

                Closed ->
                    ( state, Cmd.batch [ cmd, focusOnItem args.config.id newFocus args.msgConfig.onNoOp ] )

        Opened Menus.Focus.Lost ->
            ( state, focusOnControl args.config.id args.msgConfig.onNoOp )

        Closed ->
            ( Closed, cmd )


type State
    = Opened (Menus.Focus.Focus Int)
    | Closed


init : State
init =
    Closed


closed : { config : Config options, msgConfig : MsgConfig msg } -> ( State, Cmd msg )
closed _ =
    ( Closed, Cmd.none )


opened : OpenDirection -> { config : Config options, state : State, msgConfig : MsgConfig msg, options : options } -> ( State, Cmd msg )
opened openDirection args =
    case args.state of
        Opened value ->
            ( Opened value
            , Cmd.none
            )

        Closed ->
            case openDirection of
                Top ->
                    ( Opened (Menus.Focus.On 0)
                    , focusOnItem args.config.id 0 args.msgConfig.onNoOp
                    )

                Bottom ->
                    ( Opened (Menus.Focus.On (args.config.optionsLength args.options - 1))
                    , focusOnItem args.config.id (args.config.optionsLength args.options - 1) args.msgConfig.onNoOp
                    )


currentlyFocussed : State -> Maybe Int
currentlyFocussed state =
    case state of
        Opened focus ->
            Menus.Focus.Internal.toMaybe focus

        Closed ->
            Nothing


type alias MsgConfig msg =
    { onOpened : OpenDirection -> msg
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
                Menus.Focus.Internal.toMaybe focus

            Closed ->
                Nothing
    }


focusOnControl : String -> msg -> Cmd msg
focusOnControl id onNoOp =
    Browser.Dom.focus (ids.control id)
        |> Task.attempt (always onNoOp)


focusOnItem : String -> Int -> msg -> Cmd msg
focusOnItem id idx onNoOp =
    Browser.Dom.focus (ids.option id (String.fromInt idx))
        |> Task.attempt (always onNoOp)


succeedIfClickIsOustideOfId : List String -> Json.Decode.Decoder ()
succeedIfClickIsOustideOfId targetIds =
    let
        succeedIfParentsHaveId : () -> Json.Decode.Decoder ()
        succeedIfParentsHaveId _ =
            Json.Decode.field "id" Json.Decode.string
                |> Json.Decode.andThen
                    (\id ->
                        if List.member id targetIds then
                            Json.Decode.succeed ()

                        else
                            Json.Decode.field "parentNode" (succeedIfParentsHaveId ())
                    )

        invertDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder ()
        invertDecoder decoder =
            Json.Decode.maybe decoder
                |> Json.Decode.andThen
                    (\maybe ->
                        if maybe == Nothing then
                            Json.Decode.succeed ()

                        else
                            Json.Decode.fail ""
                    )
    in
    succeedIfParentsHaveId ()
        |> Json.Decode.field "relatedTarget"
        |> invertDecoder


button : Token options msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
button token attributes =
    case token.state of
        Opened _ ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Aria.hasMenuPopUp
                    , Aria.expanded True
                    , Aria.controls [ ids.options token.config.id ]
                    , Events.on "focusout"
                        (Json.Decode.map
                            (always token.msgConfig.onClosed)
                            (succeedIfClickIsOustideOfId [ ids.options token.config.id, ids.control token.config.id ])
                        )
                    ]
                )

        Closed ->
            Html.button
                (List.append attributes
                    [ Attr.id (ids.control token.config.id)
                    , Attr.type_ "button"
                    , Aria.hasMenuPopUp
                    , Aria.expanded False
                    , Events.onMouseDown (token.msgConfig.onOpened Top)
                    , Menus.KeyEvent.Internal.onKeyDown
                        [ Menus.KeyEvent.Internal.down (token.msgConfig.onOpened Top)
                        , Menus.KeyEvent.Internal.up (token.msgConfig.onOpened Bottom)
                        , Menus.KeyEvent.Internal.enter (token.msgConfig.onOpened Top)
                        , Menus.KeyEvent.Internal.space (token.msgConfig.onOpened Top)
                        ]
                    ]
                )


options : Token options msg -> List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
options token attributes =
    Html.ul
        (List.append attributes
            [ Attr.id (ids.options token.config.id)
            , Role.menu
            , Aria.labelledBy (ids.control token.config.id)
            , Key.tabbable False
            , Menus.Focus.Internal.loseOnMouseLeave token.msgConfig
            , Events.on "focusout"
                (Json.Decode.map
                    (always token.msgConfig.onClosed)
                    (succeedIfClickIsOustideOfId [ ids.options token.config.id, ids.control token.config.id ])
                )
            ]
        )


type Link msg
    = Link (Html msg)


link : Token options msg -> { idx : Int } -> List (Html.Attribute msg) -> ((List (Html.Attribute msg) -> List (Html Never) -> Link msg) -> Link msg) -> Html msg
link token args attributes func =
    let
        linkChild : List (Html.Attribute msg) -> List (Html Never) -> Link msg
        linkChild linkAttributes linkChildren =
            Link
                (Html.a
                    (List.append linkAttributes
                        [ Attr.id (ids.option token.config.id (String.fromInt args.idx))
                        , Role.menuItem
                        , Key.tabbable False
                        , Events.preventDefaultOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
                        ]
                    )
                    (List.map (Html.map never) linkChildren)
                )

        (Link link_) =
            func linkChild
    in
    Html.li
        (List.append attributes
            [ Attr.attribute "role" "none"
            , Key.tabbable (Just args.idx == token.focussed)
            , Events.stopPropagationOn "mousedown" (Json.Decode.succeed ( token.msgConfig.onNoOp, True ))
            , Menus.Focus.Internal.focusOnMouseMove token.msgConfig token.focussed args.idx
            , Events.onFocus (token.msgConfig.onFocussed (Menus.Focus.Internal.FocussedSpecific args.idx))
            , Menus.KeyEvent.Internal.onKeyDown
                [ Menus.KeyEvent.Internal.escape token.msgConfig.onClosed
                , Menus.Focus.Internal.keyEvents token.msgConfig
                ]
            ]
        )
        [ link_
        ]
