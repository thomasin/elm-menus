module Menus.Focus exposing (FocusAction(..), Focus(..), fromMaybe, toMaybe)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type FocusAction
    = MovedLeft
    | MovedRight
    | MovedUp
    | MovedDown


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Focus value
    = On value
    | Lost


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
fromMaybe : Maybe value -> Focus value
fromMaybe maybeValue =
    case maybeValue of
        Just value ->
            On value

        Nothing ->
            Lost


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
toMaybe : Focus value -> Maybe value
toMaybe focus =
    case focus of
        On value ->
            Just value

        Lost ->
            Nothing