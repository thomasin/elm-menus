module Menus.Active exposing (Active(..))

{-| Little helper to define whether focus moving to an option should
also select it.

# Definition
@docs Active

-}

{-|
-}
type Active option
    = Focussed option
    | FocussedAndSelected option