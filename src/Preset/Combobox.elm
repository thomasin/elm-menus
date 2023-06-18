module Preset.Combobox exposing (Model, init, Msg, update, menuOpened, menuClosed, Config, Context, OptionConfig, VisibleOptionConfig, config, autoSelect, CreatableConfig, creatable, view, isOpen, focussedOption, options, option, input, optionsDiv, Token)

{-| to-do


# Definition

@docs Model, init, Msg, update, menuOpened, menuClosed, Config, Context, OptionConfig, VisibleOptionConfig, config, autoSelect, CreatableConfig, creatable, view, isOpen, focussedOption, options, option, input, optionsDiv, Token

-}

import Html
import List.Extra as List
import Menus.Active
import Menus.Combobox
import Menus.Focus
import Menus.Select
import Menus.Select.Internal
import Preset.Combobox.Internal


{-| to-do
-}
type Model option
    = Model
        { menu : Menus.Combobox.State option
        , justOpened : Bool
        }


{-| to-do
-}
type Context
    = Context { justOpened : Bool }


{-| to-do
-}
type alias Msg option =
    Preset.Combobox.Internal.Msg option


{-| to-do
-}
init : Model option
init =
    Model
        { menu = Menus.Combobox.init
        , justOpened = False
        }


{-| to-do
-}
update : Msg option -> { model : Model option, config : Context -> Config option selection, options : List option, selected : selection } -> ( selection, Model option, Cmd (Msg option) )
update msg args =
    let
        (Model model) =
            args.model
    in
    case msg of
        Preset.Combobox.Internal.MenuOpened ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = True })

                ( model_, cmd_ ) =
                    Menus.Combobox.opened
                        (Menus.Combobox.menuToken
                            { state = model.menu
                            , config = menuConfig
                            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , Model { menu = model_, justOpened = True }
            , cmd_
            )

        Preset.Combobox.Internal.MenuClosed ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = model.justOpened })

                ( model_, cmd_ ) =
                    Menus.Combobox.closed
                        (Menus.Combobox.menuToken
                            { state = model.menu
                            , config = menuConfig
                            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , Model { menu = model_, justOpened = model.justOpened }
            , cmd_
            )

        Preset.Combobox.Internal.MenuFocussed focussed ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = model.justOpened })

                ( model_, cmd_ ) =
                    Menus.Combobox.focussed focussed
                        (Menus.Combobox.menuToken
                            { state = model.menu
                            , config = menuConfig
                            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , Model { menu = model_, justOpened = model.justOpened }
            , cmd_
            )

        Preset.Combobox.Internal.MenuSelected selected ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = model.justOpened })

                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.selected selected
                        (Menus.Combobox.menuToken
                            { state = model.menu
                            , config = menuConfig
                            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( selected_
            , Model { menu = model_, justOpened = model.justOpened }
            , cmd_
            )

        Preset.Combobox.Internal.MenuInputted inputted ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = False })

                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.inputted inputted
                        (Menus.Combobox.menuToken
                            { state = model.menu
                            , config = menuConfig
                            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( selected_
            , Model { menu = model_, justOpened = False }
            , cmd_
            )

        Preset.Combobox.Internal.NoOp ->
            ( args.selected
            , args.model
            , Cmd.none
            )


{-| to-do
-}
menuClosed : Msg option
menuClosed =
    Preset.Combobox.Internal.menuMsgConfig.onClosed


{-| to-do
-}
menuOpened : Msg option
menuOpened =
    Preset.Combobox.Internal.menuMsgConfig.onOpened


{-| to-do
-}
type Config option selection
    = Config (Menus.Combobox.Config (List option) option option selection)


{-| to-do
-}
type alias OptionConfig option selection =
    { id : String
    , optionToLabel : option -> String
    , optionToId : option -> String
    , optionToSelection : option -> selection
    , selectionToOption : selection -> List option -> Maybe option
    , selectionToLabel : selection -> String
    , selectionCleared : Menus.Select.Change selection
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    , visibleOptions : VisibleOptionConfig option selection -> String -> selection -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
    }


{-| to-do
-}
config : OptionConfig option selection -> Context -> Config option selection
config optionConfig (Context context) =
    Config
        (Menus.Combobox.emptiable
            { id = optionConfig.id
            , empty = []
            , optionToValue = identity
            , valueToString = optionConfig.optionToId
            , selectionToLabel = optionConfig.selectionToLabel

            -- \maybeSelected ->
            --     case maybeSelected of
            --         Just selected ->
            --             optionConfig.selectionToLabel selected
            --         Nothing ->
            --             ""
            , selectionToOption = optionConfig.selectionToOption
            , selectChange =
                \action selected opts ->
                    let
                        selectionMatchesOpt : option -> Bool
                        selectionMatchesOpt opt =
                            selected == optionConfig.optionToSelection opt
                    in
                    case action of
                        Menus.Select.Internal.MovedLeft ->
                            case List.splitWhen selectionMatchesOpt opts of
                                Just ( previous, _ ) ->
                                    List.last previous
                                        |> Maybe.map (Menus.Select.ChangedTo << optionConfig.optionToSelection)
                                        |> Maybe.withDefault Menus.Select.NotChanged

                                Nothing ->
                                    List.last opts
                                        |> Maybe.map (Menus.Select.ChangedTo << optionConfig.optionToSelection)
                                        |> Maybe.withDefault Menus.Select.NotChanged

                        Menus.Select.Internal.MovedRight ->
                            case List.splitWhen selectionMatchesOpt opts of
                                Just ( _, _ :: os ) ->
                                    List.head os
                                        |> Maybe.map (Menus.Select.ChangedTo << optionConfig.optionToSelection)
                                        |> Maybe.withDefault Menus.Select.NotChanged

                                Just ( _, [] ) ->
                                    Menus.Select.NotChanged

                                Nothing ->
                                    List.head opts
                                        |> Maybe.map (Menus.Select.ChangedTo << optionConfig.optionToSelection)
                                        |> Maybe.withDefault Menus.Select.NotChanged

                        Menus.Select.Internal.Cleared _ ->
                            optionConfig.selectionCleared
            , selectValue =
                \value _ _ ->
                    optionConfig.optionToSelection value
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
                            if context.justOpened || str == "" then
                                case List.filter (optionConfig.matchesInput str) opts of
                                    exactMatch :: _ ->
                                        Just ( Menus.Active.Focussed exactMatch, opts )

                                    [] ->
                                        case List.filter (optionConfig.containsInput str) opts of
                                            match :: _ ->
                                                Just ( Menus.Active.Focussed match, opts )

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
                        , optionToSelection = optionConfig.optionToSelection
                        , selectionToOption = optionConfig.selectionToOption
                        , selectionToLabel = optionConfig.selectionToLabel
                        , matchesInput = optionConfig.matchesInput
                        , containsInput = optionConfig.containsInput
                        }
                        str
                        selected
                        visibleOpts
            }
        )


{-| to-do
-}
type alias VisibleOptionConfig option selection =
    { optionToLabel : option -> String
    , optionToId : option -> String
    , optionToSelection : option -> selection
    , selectionToOption : selection -> List option -> Maybe option
    , selectionToLabel : selection -> String
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    }


{-| to-do
-}
autoSelect : VisibleOptionConfig option selection -> String -> selection -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
autoSelect _ _ _ visibleOpts =
    case visibleOpts of
        Just ( Menus.Active.Focussed match, matches ) ->
            Just ( Menus.Active.FocussedAndSelected match, matches )

        Just ( Menus.Active.FocussedAndSelected match, matches ) ->
            Just ( Menus.Active.FocussedAndSelected match, matches )

        Nothing ->
            Nothing


{-| to-do
-}
type alias CreatableConfig option =
    { newOption : String -> option
    }


{-| to-do
-}
creatable : CreatableConfig option -> VisibleOptionConfig option selection -> String -> selection -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
creatable creatableConfig optionConfig str _ visibleOpts =
    if str == "" then
        visibleOpts

    else
        case visibleOpts of
            Just ( Menus.Active.Focussed _, matches ) ->
                case List.filter (optionConfig.matchesInput str) matches of
                    -- If there are any options that exactly match the input
                    exactMatch :: _ ->
                        Just ( Menus.Active.Focussed exactMatch, matches )

                    -- If there are no exact matches, and the user is possibly creating something new
                    [] ->
                        Just ( Menus.Active.Focussed (creatableConfig.newOption str), creatableConfig.newOption str :: matches )

            Just ( Menus.Active.FocussedAndSelected _, matches ) ->
                case List.filter (optionConfig.matchesInput str) matches of
                    -- If there are any options that exactly match the input
                    exactMatch :: _ ->
                        Just ( Menus.Active.FocussedAndSelected exactMatch, matches )

                    -- If there are no exact matches, and the user is possibly creating something new
                    [] ->
                        Just ( Menus.Active.FocussedAndSelected (creatableConfig.newOption str), creatableConfig.newOption str :: matches )

            Nothing ->
                Just ( Menus.Active.Focussed (creatableConfig.newOption str), [ creatableConfig.newOption str ] )


{-| to-do
-}
type Token option selection
    = Token (Menus.Combobox.Token (List option) option option selection (Msg option))


{-| to-do
-}
isOpen : Token option selection -> Bool
isOpen (Token token) =
    Menus.Combobox.isOpen token.state


{-| to-do
-}
focussedOption : Token option selection -> Maybe option
focussedOption (Token token) =
    token.focussed


{-| to-do
-}
view : { model : Model option, config : Context -> Config option selection, options : List option, selected : selection } -> (Token option selection -> List option -> Html.Html (Msg option)) -> Html.Html (Msg option)
view args func =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config (Context { justOpened = model.justOpened })

        unwrappedToken : Menus.Combobox.Token (List option) option option selection (Msg option)
        unwrappedToken =
            Menus.Combobox.menuToken
                { state = model.menu
                , config = menuConfig
                , msgConfig = Preset.Combobox.Internal.menuMsgConfig
                , options = args.options
                , selected = args.selected
                }
    in
    func (Token unwrappedToken) (Maybe.withDefault [] unwrappedToken.visibleOptions)


{-| The input used to open and close the menu
-}
input : Token option selection -> ({ placeholder : String } -> List (Html.Attribute (Msg option)) -> Html.Html (Msg option))
input (Token token) =
    Menus.Combobox.input token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
options : Token option selection -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
options (Token token) =
    Menus.Combobox.options token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
optionsDiv : Token option selection -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
optionsDiv (Token token) =
    Menus.Combobox.optionsDiv token


{-| A focus and selectable option
-}
option : Token option selection -> ({ value : option, isSelected : Bool } -> List (Html.Attribute (Msg option)) -> List (Html.Html Never) -> Html.Html (Msg option))
option (Token token) =
    Menus.Combobox.option token
