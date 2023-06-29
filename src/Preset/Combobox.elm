module Preset.Combobox exposing (Model, init, Msg, update, menuOpened, menuClosed, Config, Context, OptionConfig, VisibleOptionConfig, config, autoSelect, CreatableConfig, creatable, view, isOpen, focussedOption, options, option, input, optionsDiv, Token, token)

{-| A custom combobox, with support for keyboard navigation
This Preset can be used for basic comboboxes, without complex selection models
or option/value relationships.

@docs Model, Context, init, Msg, menuOpened, menuClosed, update

## Configuration

@docs Config, OptionConfig, config, VisibleOptionConfig, autoSelect, CreatableConfig, creatable

## Views

@docs view, isOpen, focussedOption, options, option, input, optionsDiv, Token, token
-}

import Html
import List.Extra as List
import Menus.Active
import Menus.Combobox
import Menus.Focus
import Menus.Select
import Menus.Select.Internal
import Preset.Combobox.Internal


{-| Stores whether the menu is open or not, and which option is currently focussed.  
    It does _not_ store which option is selected.  
    Is updated and returned in the [#update](#update) function.  
-}
type Model option
    = Model
        { menu : Menus.Combobox.State option
        , justOpened : Bool
        }


{-| Context is passed around internally, and is just used to track if the menu has been recently opened
-}
type Context
    = Context { justOpened : Bool }


{-| Passed in to [#update](#update)  
-}
type alias Msg option =
    Preset.Combobox.Internal.Msg option


{-| Initialise a closed menu, you can use this in your own init function
-}
init : Model option
init =
    Model
        { menu = Menus.Combobox.init
        , justOpened = False
        }


{-| Call this in your own update function, it returns the latest selection, 
    the dropdown model, and `Cmd`s to pass on. You will need to save the selection and menu in your model.
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


{-| A [#Msg](#Msg) that is sent when the menu is closed
-}
menuClosed : Msg option
menuClosed =
    Preset.Combobox.Internal.menuMsgConfig.onClosed


{-| A [#Msg](#Msg) that is sent when the menu is opened
-}
menuOpened : Msg option
menuOpened =
    Preset.Combobox.Internal.menuMsgConfig.onOpened


{-| Created in [#config](#config)  
    This is passed into [#update](#update) and [#view](#view) and is used to control how the menu reacts to user input
-}
type Config option selection
    = Config (Menus.Combobox.Config (List option) option option selection)


{-| Passed in to [#config](#config)  
    - `id`: Must be unique, and is displayed in the HTML, used for accessibility  
    - `optionToLabel`: Allows users to use keyboard input to search for options  
    - `optionToId`: Uniquely identify each option
    - `optionToSelection`: Turn any option into your selection, for instance you might want the selection to be `Maybe option`,
        so you would have `optionToSelection = Just` 
    - `selectionToOption`: Given the current selection and a list of options, find the option that corresponds to the selection.
    - `selectionToLabel`: What to set as the menu input's value
    - `selectionCleared`: When the user presses backspace, while the menu is closed or the input is empty, what should happen to the selection?
        Returning `NotChanged` will make the combobox non-clearable.
    - `matchesInput`: Given a string and an option, does the option exactly match the string?
    - `containsInput`: Given a string and an option, does the option "contain" the string?
    - `visibleOptions`: The Preset does some work under the scenes to already filter down visible options using data passed in from this config object,
but there are a couple of "plugins", [#autoSelect](#autoSelect) and [#creatable](#creatable) you can use here to change how the combobox functions.
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


{-| Create a [#Config](#Config) type. You don't need to explicitly pass in the `Context` yourself,
    the [#update](#update) and [#view](#view) functions expect `Context -> Config option selection`.
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


{-| This is passed down to the [#autoSelect](#autoSelect) and [#creatable](#creatable) plugins from the main config
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


{-| This plugin will automatically select the closest match to the input.
    If you are combining this with other plugins, use it last or any changes within the latter plugins won't be autoselected.


    Preset.Combobox.config
        { ...
        , visibleOptions =
            Preset.Combobox.autoSelect
        }
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


{-| Passed into [#creatable](#creatable)
    Define how to create a new option, given a string
-}
type alias CreatableConfig option =
    { newOption : String -> option
    }


{-| This plugin allows people to create new options when they enter input that doesn't exactly match any of the options.


    Preset.Combobox.config
        { ...
        , visibleOptions =
            Preset.Combobox.creatable
                { newOption = \label -> NewOption label }
        }
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


{-| Created automatically when you call [#view](#view), pass it in to view functions
-}
type Token option selection
    = Token (Menus.Combobox.Token (List option) option option selection (Msg option))


{-| Create a token manually for use in view and helper functions
-}
token : { model : Model option, config : Context -> Config option selection, options : List option, selected : selection } -> Token option selection
token args =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config (Context { justOpened = model.justOpened })
    in
    Token
        (Menus.Combobox.menuToken
            { state = model.menu
            , config = menuConfig
            , msgConfig = Preset.Combobox.Internal.menuMsgConfig
            , options = args.options
            , selected = args.selected
            }
        )


{-| Check whether the menu is open or closed
-}
isOpen : Token option selection -> Bool
isOpen (Token token_) =
    Menus.Combobox.isOpen token_.state


{-| Returns the currently focussed option
-}
focussedOption : Token option selection -> Maybe option
focussedOption (Token token_) =
    token_.focussed


{-| This needs to wrap your menu. It does not render any HTML, but provides a [#Token](#Token) that you pass into view functions, and also the currently visible options.
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
input (Token token_) =
    Menus.Combobox.input token_


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
options : Token option selection -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
options (Token token_) =
    Menus.Combobox.options token_


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
optionsDiv : Token option selection -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
optionsDiv (Token token_) =
    Menus.Combobox.optionsDiv token_


{-| A focus and selectable option
-}
option : Token option selection -> ({ value : option, isSelected : Bool } -> List (Html.Attribute (Msg option)) -> List (Html.Html Never) -> Html.Html (Msg option))
option (Token token_) =
    Menus.Combobox.option token_
