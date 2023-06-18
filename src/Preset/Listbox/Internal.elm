module Preset.Listbox.Internal exposing (Msg(..), menuMsgConfig)

import Menus.Listbox


type Msg
    = MenuOpened Menus.Listbox.OpenDirection
    | MenuClosed
    | MenuFocussed (Menus.Listbox.Focussed Int)
    | MenuSelected (Menus.Listbox.Selected Int)
    | MenuInputted Menus.Listbox.Inputted
    | NoOp


menuMsgConfig : Menus.Listbox.MsgConfig Int Msg
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onSelected = MenuSelected
    , onInputted = MenuInputted
    , onNoOp = NoOp
    }
