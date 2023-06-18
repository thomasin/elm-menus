module Preset.Combobox.Internal exposing (Msg(..), menuMsgConfig)

import Menus.Combobox


{-| to-do
-}
type Msg option
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Combobox.Focussed option)
    | MenuSelected (Menus.Combobox.Selected option)
    | MenuInputted Menus.Combobox.Inputted
    | NoOp


{-| to-do
-}
menuMsgConfig : Menus.Combobox.MsgConfig option (Msg option)
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onSelected = MenuSelected
    , onInput = MenuInputted
    , onNoOp = NoOp
    }
