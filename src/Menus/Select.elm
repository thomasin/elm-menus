module Menus.Select exposing (Change(..))

{-| Define whether a selection should change or not

@docs Change

-}


{-| Returned from `selectionCleared` in [#OptionConfig](#OptionConfig)
-}
type Change selection
    = ChangedTo selection
    | NotChanged
