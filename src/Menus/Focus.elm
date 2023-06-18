module Menus.Focus exposing (FocusAction(..), Focus(..), fromMaybe)

{-| to-do


# Definition

@docs FocusAction, Focus, fromMaybe

-}


{-| to-do
-}
type FocusAction
    = MovedLeft
    | MovedRight
    | MovedUp
    | MovedDown


{-| to-do
-}
type Focus value
    = On value
    | Lost


{-| to-do
-}
fromMaybe : Maybe value -> Focus value
fromMaybe maybeValue =
    case maybeValue of
        Just value ->
            On value

        Nothing ->
            Lost
