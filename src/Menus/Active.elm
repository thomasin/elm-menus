module Menus.Active exposing (Active(..))

{-| Little helper to define whether focus moving to an option on input change should
also select it.


# Definition

@docs Active

-}


{-| This will focus the first matching option but not select it when input is changed

    { config...
    , visibleOptions =
        \str selected opts ->
            case List.head (List.filter ((==) str) opts) of
                Just match ->
                    Just ( Menus.Active.Focussed match, opts )

                Nothing ->
                    Nothing
    }

This will focus and select the first matching option when input is changed

    { config...
    , visibleOptions =
        \str selected opts ->
            case List.head (List.filter ((==) str) opts) of
                Just match ->
                    Just ( Menus.Active.FocussedAndSelected opt, opts )

                Nothing ->
                    Nothing
    }

-}
type Active option
    = Focussed option
    | FocussedAndSelected option
