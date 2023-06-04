module Preset.Combobox exposing (..)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs map, withDefault, oneOf

# Chaining Maybes
@docs andThen

-}

import Html
import Html.Attributes as Attr
import Html.Events
import List.Extra as List
import Menus.Combobox
import Menus.Select
import Menus.Focus
import Menus.Active


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Model option =
    { menu : Menus.Combobox.State option
    , selected : Maybe option
    , options : List option
    , justOpened : Bool
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
init : List option -> Maybe option -> Model option
init options initialSelection =
    { menu = Menus.Combobox.init
    , selected = initialSelection
    , options = options
    , justOpened = False
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type Msg option
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Combobox.Focussed option)
    | MenuSelected (Menus.Combobox.Selected option)
    | MenuInputted Menus.Combobox.Inputted
    | NoOp


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
update : Msg option -> Model option -> (Bool -> Config option) -> ( Model option, Cmd (Msg option) )
update msg model menuConfig =
    case msg of
        MenuOpened ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.opened
                        { state = model.menu
                        , config = (menuConfig True)
                        , msgConfig = menuMsgConfig
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, justOpened = True }
            , cmd_
            )

        MenuClosed ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.closed
                        { config = (menuConfig model.justOpened)
                        , msgConfig = menuMsgConfig
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuFocussed focussed ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.focussed
                        { msg = focussed
                        , state = model.menu
                        , config = (menuConfig model.justOpened)
                        , msgConfig = menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuSelected selected ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.selected
                        { msg = selected
                        , state = model.menu
                        , config = (menuConfig model.justOpened)
                        , msgConfig = menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        MenuInputted inputted ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = (menuConfig False)
                        , msgConfig = menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_, justOpened = False }
            , cmd_
            )

        NoOp ->
            ( model, Cmd.none )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
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


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
menuClosed : Msg option
menuClosed =
    menuMsgConfig.onClosed


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
menuOpened : Msg option
menuOpened =
    menuMsgConfig.onOpened


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias Config option =
    Menus.Combobox.Config (List option) option option (Maybe option)


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias OptionConfig option =
    { id : String
    , optionToLabel : option -> String
    , optionToId : option -> String
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    , visibleOptions : VisibleOptionFuncConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias VisibleOptionFuncConfig option =
    { optionToLabel : option -> String
    , optionToId : option -> String
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
config : Bool -> OptionConfig option -> Config option
config justOpened optionConfig =
    Menus.Combobox.emptiable
        { id = optionConfig.id
        , empty = []
        , optionToLabel = optionConfig.optionToLabel
        , optionToValue = identity
        , valueToString = optionConfig.optionToId
        , selectionToLabel = \maybeSelected ->
            case maybeSelected of
                Just opt ->
                    optionConfig.optionToLabel opt

                Nothing ->
                    ""
        , selectionToOption = identity
        , selectChange =
            \action maybeSelected opts ->
                case action of
                    Menus.Select.MovedLeft ->
                        case maybeSelected of
                            Just selected_ ->
                                case List.splitWhen ((==) selected_) opts of
                                    Just ( previous, _ ) ->
                                        Menus.Select.ChangedTo (List.last previous)

                                    Nothing ->
                                        Menus.Select.ChangedTo (List.last opts)

                            Nothing ->
                                Menus.Select.ChangedTo (List.last opts)

                    Menus.Select.MovedRight ->
                        case maybeSelected of
                            Just selected_ ->
                                case List.splitWhen ((==) selected_) opts of
                                    Just ( _, _ :: os ) ->
                                        Menus.Select.ChangedTo (List.head os)

                                    Just ( _, [] ) ->
                                        Menus.Select.NotChanged

                                    Nothing ->
                                        Menus.Select.ChangedTo (List.head opts)

                            Nothing ->
                                Menus.Select.ChangedTo (List.head opts)

                    Menus.Select.Cleared ->
                        Menus.Select.ChangedTo Nothing
        , selectValue =
            \value _ _ ->
                Just value
        , focusChange =
            \action value opts ->
                case action of
                    Menus.Focus.MovedUp ->
                        case value of
                            Menus.Focus.On currentFocus ->
                                case List.splitWhen (\opt -> opt == currentFocus) opts of
                                    Just ( previous, _ ) ->
                                        Menus.Focus.fromMaybe (List.last previous)

                                    Nothing ->
                                        Menus.Focus.fromMaybe (List.last opts)

                            Menus.Focus.Lost ->
                                Menus.Focus.fromMaybe (List.last opts)

                    Menus.Focus.MovedDown ->
                        case value of
                            Menus.Focus.On currentFocus ->
                                case List.splitWhen (\opt -> opt == currentFocus) opts of
                                    Just ( _, _ :: os ) ->
                                        Menus.Focus.fromMaybe (List.head os)

                                    Just ( _, [] ) ->
                                        Menus.Focus.Lost

                                    Nothing ->
                                        Menus.Focus.fromMaybe (List.head opts)

                            Menus.Focus.Lost ->
                                Menus.Focus.fromMaybe (List.head opts)

                    Menus.Focus.MovedLeft ->
                        Menus.Focus.Lost

                    Menus.Focus.MovedRight ->
                        Menus.Focus.Lost
        , visibleOptions =
            \str selected opts ->
                let
                    visibleOpts : Maybe ( Menus.Active.Active option, List option )
                    visibleOpts =
                        if justOpened || str == "" then
                            case List.filter (optionConfig.matchesInput str) opts of
                                exactMatch :: _ ->
                                    Just ( Menus.Active.Focussed exactMatch, opts )

                                [] ->
                                    case List.head opts of
                                        Just firstOpt ->
                                            Just ( Menus.Active.Focussed firstOpt, opts )

                                        Nothing ->
                                            Nothing

                        else
                            case List.filter (optionConfig.containsInput str) opts of
                                match :: matches ->
                                    Just ( Menus.Active.Focussed match, match :: matches )

                                [] ->
                                    Nothing  
                in
                optionConfig.visibleOptions
                    { optionToLabel = optionConfig.optionToLabel
                    , optionToId = optionConfig.optionToId
                    , matchesInput = optionConfig.matchesInput
                    , containsInput = optionConfig.containsInput
                    }
                    str
                    selected
                    visibleOpts
        }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
autoSelect : VisibleOptionFuncConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
autoSelect optionConfig str selected visibleOpts =
    case visibleOpts of
        Just ( Menus.Active.Focussed match, matches ) ->
            Just ( Menus.Active.FocussedAndSelected match, matches )

        Just ( Menus.Active.FocussedAndSelected match, matches ) ->
            Just ( Menus.Active.FocussedAndSelected match, matches )

        Nothing ->
            Nothing



{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
type alias CreatableConfig option =
    { newOption : String -> option
    }


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
creatable : CreatableConfig option -> VisibleOptionFuncConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
creatable creatableConfig optionConfig str selected visibleOpts =
    if str == "" then
        visibleOpts

    else
        case visibleOpts of
            Just ( Menus.Active.Focussed match, matches ) ->
                case List.filter (optionConfig.matchesInput str) matches of
                    -- If there are any options that exactly match the input
                    exactMatch :: _ ->
                        Just ( Menus.Active.Focussed exactMatch, matches )

                    -- If there are no exact matches, and the user is possibly creating something new
                    [] ->
                        Just ( Menus.Active.Focussed (creatableConfig.newOption str), creatableConfig.newOption str :: matches )

            Just ( Menus.Active.FocussedAndSelected match, matches ) ->
                case List.filter (optionConfig.matchesInput str) matches of
                    -- If there are any options that exactly match the input
                    exactMatch :: _ ->
                        Just ( Menus.Active.FocussedAndSelected exactMatch, matches )

                    -- If there are no exact matches, and the user is possibly creating something new
                    [] ->
                        Just ( Menus.Active.FocussedAndSelected (creatableConfig.newOption str), creatableConfig.newOption str :: matches )

            Nothing ->
                ( Just ( Menus.Active.Focussed (creatableConfig.newOption str), [ creatableConfig.newOption str ] ) )


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    fromList ['e','l','m'] == "elm"
-}
view : Model option -> (Bool -> Config option) -> (Menus.Combobox.Token (List option) option option (Maybe option) (Msg option) -> List option -> Html.Html (Msg option)) -> Html.Html (Msg option)
view model menuConfig func =
    let
        token : Menus.Combobox.Token (List option) option option (Maybe option) (Msg option)
        token =
            Menus.Combobox.menuToken
                { state = model.menu
                , config = menuConfig model.justOpened
                , msgConfig = menuMsgConfig
                , selected = model.selected
                }

    in
    func token
        (Maybe.withDefault []
            (Menus.Combobox.visibleOptions
                { state = model.menu
                , config = menuConfig model.justOpened
                , selected = model.selected
                , options = model.options
                }
            )
        )