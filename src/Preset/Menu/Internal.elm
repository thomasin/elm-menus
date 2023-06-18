module Preset.Menu.Internal exposing (Msg(..), MsgConfig(..), menuMsgConfig)

import Menus.Menu


type Msg
    = MenuOpened Menus.Menu.OpenDirection
    | MenuClosed
    | MenuFocussed Menus.Menu.Focussed
    | NoOp


type MsgConfig
    = MsgConfig (Menus.Menu.MsgConfig Msg)


menuMsgConfig : MsgConfig
menuMsgConfig =
    MsgConfig
        { onOpened = MenuOpened
        , onClosed = MenuClosed
        , onFocussed = MenuFocussed
        , onNoOp = NoOp
        }
