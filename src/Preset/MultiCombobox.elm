module Preset.MultiCombobox exposing (Model, init, Msg, update, menuOpened, menuClosed, Config, OptionConfig, VisibleOptionConfig, config, autoSelect, CreatableConfig, creatable, view, options, option, optionsDiv, input, Token, allSelectionsRemoved, selectionRemoved, lastSelectionRemoved, isOpen, focussedOption, Context)

{-| to-do

@docs Model, init, Msg, update, menuOpened, menuClosed, Config, OptionConfig, VisibleOptionConfig, config, autoSelect, CreatableConfig, creatable, view, options, option, optionsDiv, input, Token, allSelectionsRemoved, selectionRemoved, lastSelectionRemoved, isOpen, focussedOption, Context

-}

import Html
import List.Extra as List
import Menus.Active
import Menus.Combobox
import Menus.Focus
import Menus.Select
import Menus.Select.Internal
import Preset.MultiCombobox.Internal


{-| to-do
-}
type alias Model option =
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
    Preset.MultiCombobox.Internal.Msg option


{-| to-do
-}
init : Model option
init =
    { menu = Menus.Combobox.init
    , justOpened = False
    }


{-| to-do
-}
update : Msg option -> { model : Model option, config : Context -> Config option, options : List option, selected : List option } -> ( List option, Model option, Cmd (Msg option) )
update msg args =
    case msg of
        Preset.MultiCombobox.Internal.SelectionRemoved opt ->
            ( List.filter ((/=) opt) args.selected
            , args.model
            , Cmd.none
            )

        Preset.MultiCombobox.Internal.LastSelectionRemoved ->
            ( List.drop 1 args.selected
            , args.model
            , Cmd.none
            )

        Preset.MultiCombobox.Internal.AllSelectionsRemoved ->
            ( []
            , args.model
            , Cmd.none
            )

        Preset.MultiCombobox.Internal.MenuOpened ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = True })

                ( model_, cmd_ ) =
                    Menus.Combobox.opened
                        (Menus.Combobox.menuToken
                            { state = args.model.menu
                            , config = menuConfig
                            , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , { menu = model_, justOpened = True }
            , cmd_
            )

        Preset.MultiCombobox.Internal.MenuClosed ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = args.model.justOpened })

                ( model_, cmd_ ) =
                    Menus.Combobox.closed
                        (Menus.Combobox.menuToken
                            { state = args.model.menu
                            , config = menuConfig
                            , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , { menu = model_, justOpened = args.model.justOpened }
            , cmd_
            )

        Preset.MultiCombobox.Internal.MenuFocussed focussed ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = args.model.justOpened })

                ( model_, cmd_ ) =
                    Menus.Combobox.focussed focussed
                        (Menus.Combobox.menuToken
                            { state = args.model.menu
                            , config = menuConfig
                            , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( args.selected
            , { menu = model_, justOpened = args.model.justOpened }
            , cmd_
            )

        Preset.MultiCombobox.Internal.MenuSelected selected ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = args.model.justOpened })

                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.selected selected
                        (Menus.Combobox.menuToken
                            { state = args.model.menu
                            , config = menuConfig
                            , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( selected_
            , { menu = model_, justOpened = args.model.justOpened }
            , cmd_
            )

        Preset.MultiCombobox.Internal.MenuInputted inputted ->
            let
                (Config menuConfig) =
                    args.config (Context { justOpened = False })

                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.inputted inputted
                        (Menus.Combobox.menuToken
                            { state = args.model.menu
                            , config = menuConfig
                            , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                            , options = args.options
                            , selected = args.selected
                            }
                        )
            in
            ( selected_
            , { menu = model_, justOpened = False }
            , cmd_
            )

        Preset.MultiCombobox.Internal.NoOp ->
            ( args.selected
            , args.model
            , Cmd.none
            )


{-| to-do
-}
menuClosed : Msg option
menuClosed =
    Preset.MultiCombobox.Internal.menuMsgConfig.onClosed


{-| to-do
-}
menuOpened : Msg option
menuOpened =
    Preset.MultiCombobox.Internal.menuMsgConfig.onOpened


{-| to-do
-}
selectionRemoved : option -> Msg option
selectionRemoved =
    Preset.MultiCombobox.Internal.SelectionRemoved


{-| to-do
-}
lastSelectionRemoved : Msg option
lastSelectionRemoved =
    Preset.MultiCombobox.Internal.LastSelectionRemoved


{-| to-do
-}
allSelectionsRemoved : Msg option
allSelectionsRemoved =
    Preset.MultiCombobox.Internal.AllSelectionsRemoved


{-| to-do
-}
type Config option
    = Config (Menus.Combobox.Config (List option) option option (List option))


{-| to-do
-}
type alias OptionConfig option =
    { id : String
    , optionToLabel : option -> String
    , optionToId : option -> String
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    , visibleOptions : VisibleOptionConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
    }


{-| to-do
-}
type alias VisibleOptionConfig option =
    { optionToLabel : option -> String
    , optionToId : option -> String
    , matchesInput : String -> option -> Bool
    , containsInput : String -> option -> Bool
    }


{-| to-do
-}
config : OptionConfig option -> Context -> Config option
config optionConfig (Context context) =
    Config
        (Menus.Combobox.emptiable
            { id = optionConfig.id
            , empty = []
            , optionToValue = identity
            , valueToString = optionConfig.optionToId
            , selectionToLabel =
                \_ ->
                    ""
            , selectionToOption =
                \selected _ ->
                    List.head selected
            , selectChange =
                \action selected _ ->
                    case action of
                        Menus.Select.Internal.MovedLeft ->
                            Menus.Select.NotChanged

                        Menus.Select.Internal.MovedRight ->
                            Menus.Select.NotChanged

                        Menus.Select.Internal.Cleared _ ->
                            Menus.Select.ChangedTo (List.drop 1 selected)
            , selectValue =
                \value selected _ ->
                    value :: List.filter ((/=) value) selected
            , focusChange =
                \direction value opts ->
                    case direction of
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
                        unselectedOpts : List option
                        unselectedOpts =
                            List.filter (\opt -> not (List.member opt selected)) opts
                    in
                    if context.justOpened || str == "" then
                        case List.filter (optionConfig.matchesInput str) unselectedOpts of
                            exactMatch :: _ ->
                                Just ( Menus.Active.Focussed exactMatch, unselectedOpts )

                            [] ->
                                case List.filter (optionConfig.containsInput str) unselectedOpts of
                                    match :: _ ->
                                        Just ( Menus.Active.Focussed match, unselectedOpts )

                                    [] ->
                                        case List.head unselectedOpts of
                                            Just firstOpt ->
                                                Just ( Menus.Active.Focussed firstOpt, unselectedOpts )

                                            Nothing ->
                                                Nothing

                    else
                        case List.filter (optionConfig.containsInput str) unselectedOpts of
                            match :: matches ->
                                Just ( Menus.Active.Focussed match, match :: matches )

                            [] ->
                                Nothing
            }
        )


{-| to-do
-}
autoSelect : VisibleOptionConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
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
creatable : CreatableConfig option -> VisibleOptionConfig option -> String -> Maybe option -> Maybe ( Menus.Active.Active option, List option ) -> Maybe ( Menus.Active.Active option, List option )
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


type Token option
    = Token (Menus.Combobox.Token (List option) option option (List option) (Msg option))


isOpen : Token option -> Bool
isOpen (Token token) =
    Menus.Combobox.isOpen token.state


focussedOption : Token option -> Maybe option
focussedOption (Token token) =
    token.focussed


{-| to-do
-}
view : { model : Model option, config : Context -> Config option, options : List option, selected : List option } -> (Token option -> List option -> Html.Html (Msg option)) -> Html.Html (Msg option)
view args func =
    let
        (Config menuConfig) =
            args.config (Context { justOpened = args.model.justOpened })

        unwrappedToken : Menus.Combobox.Token (List option) option option (List option) (Msg option)
        unwrappedToken =
            Menus.Combobox.menuToken
                { state = args.model.menu
                , config = menuConfig
                , msgConfig = Preset.MultiCombobox.Internal.menuMsgConfig
                , options = args.options
                , selected = args.selected
                }
    in
    func (Token unwrappedToken) (Maybe.withDefault [] unwrappedToken.visibleOptions)


{-| The input used to open and close the menu
-}
input : Token option -> ({ placeholder : String } -> List (Html.Attribute (Msg option)) -> Html.Html (Msg option))
input (Token token) =
    Menus.Combobox.input token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
options : Token option -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
options (Token token) =
    Menus.Combobox.options token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
optionsDiv : Token option -> (List (Html.Attribute (Msg option)) -> List (Html.Html (Msg option)) -> Html.Html (Msg option))
optionsDiv (Token token) =
    Menus.Combobox.optionsDiv token


{-| A focus and selectable option
-}
option : Token option -> ({ value : option, isSelected : Bool } -> List (Html.Attribute (Msg option)) -> List (Html.Html Never) -> Html.Html (Msg option))
option (Token token) =
    Menus.Combobox.option token
