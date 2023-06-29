module Preset.Listbox exposing (Model, Msg, OptionConfig, Config, init, update, Token, view, config, options, button, option, isOpen, focussedOption)

{-| A custom <select>, with support for keyboard navigation
This is an attempt to reimplement the native <select> element with custom
styling. This a basic first step, that doesn't include support for multi-select,
always open listboxes, or grouped options.

@docs Model, init, Msg, update

## Configuration

@docs Config, OptionConfig, config

## View

@docs Token, isOpen, focussedOption, view, options, button, option

-}

import Html
import Menus.Focus
import Menus.Listbox
import Menus.Select
import Menus.Select.Internal
import Preset.Listbox.Internal
import ZipList



--


{-| Stores whether the menu is open or not, and which option is currently focussed.  
    It does _not_ store which option is selected.  
    Is updated and returned in the [#update](#update) function.  
-}
type Model
    = Model (Menus.Listbox.State Int)


{-| Created in [#config](#config)  
    This is passed into [#update](#update) and [#view](#view) and is used to control how the menu reacts to user input
-}
type Config option
    = Config (Menus.Listbox.Config (ZipList.ZipList option) option Int (ZipList.ZipList option))


{-| Passed in to [#update](#update)  
-}
type alias Msg =
    Preset.Listbox.Internal.Msg


{-| Passed in to [#config](#config)  
    The `id` field must be unique, and is displayed in the HTML, used for accessibility  
    The `optionToLabel` field allows users to use keyboard input to search for options
-}
type alias OptionConfig option =
    { id : String
    , optionToLabel : option -> String
    }


{-| Create a [#Config](#Config) type  
-}
config : OptionConfig option -> Config option
config optionConfig =
    let
        findIndex : (a -> Bool) -> List a -> Maybe Int
        findIndex =
            findIndexHelp 0

        findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
        findIndexHelp index predicate list =
            case list of
                [] ->
                    Nothing

                x :: xs ->
                    if predicate x then
                        Just index

                    else
                        findIndexHelp (index + 1) predicate xs
    in
    Config
        { id = optionConfig.id
        , optionToLabel = optionConfig.optionToLabel
        , optionToValue =
            \opt opts ->
                ZipList.toList opts
                    |> findIndex ((==) opt)
                    |> Maybe.withDefault 0
        , firstOption =
            \opts ->
                ZipList.goToStart opts
                    |> ZipList.current
        , lastOption =
            \opts ->
                ZipList.goToEnd opts
                    |> ZipList.current
        , valueToString = String.fromInt
        , selectionToOption = Just << ZipList.current
        , selectChange =
            \action _ opts ->
                case action of
                    Menus.Select.Internal.MovedLeft ->
                        Menus.Select.ChangedTo (ZipList.backward opts)

                    Menus.Select.Internal.MovedRight ->
                        Menus.Select.ChangedTo (ZipList.forward opts)

                    Menus.Select.Internal.Cleared _ ->
                        Menus.Select.NotChanged
        , selectValue =
            \idx _ opts ->
                ZipList.goToIndex idx opts
                    |> Maybe.withDefault opts
        , focusChange =
            \action maybePreviousIdx opts ->
                case action of
                    Menus.Focus.MovedUp ->
                        case maybePreviousIdx of
                            Just previousIdx ->
                                Menus.Focus.On (max (previousIdx - 1) 0)

                            Nothing ->
                                Menus.Focus.On (ZipList.length opts - 1)

                    Menus.Focus.MovedDown ->
                        case maybePreviousIdx of
                            Just previousIdx ->
                                Menus.Focus.On (min (previousIdx + 1) (ZipList.length opts - 1))

                            Nothing ->
                                Menus.Focus.On 0

                    Menus.Focus.MovedLeft ->
                        Menus.Focus.Lost

                    Menus.Focus.MovedRight ->
                        Menus.Focus.Lost
        , focusMatch =
            \str opts ->
                List.filter (String.startsWith (String.toLower str) << String.toLower << optionConfig.optionToLabel) (ZipList.toList opts)
                    |> List.head
        }


{-| Initialise a closed menu, you can use this in your own init function
-}
init : Model
init =
    Model Menus.Listbox.init


{-| Call this in your own update function, it returns the list of options with the correct selection, 
    the dropdown model, and `Cmd`s to pass on. You will need to save the updated options and menu in your model.
-}
update : Msg -> { model : Model, config : Config option, options : ZipList.ZipList option } -> ( ZipList.ZipList option, Model, Cmd Msg )
update msg args =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config
    in
    case msg of
        Preset.Listbox.Internal.MenuOpened openDirection ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.opened openDirection
                        { state = model
                        , config = menuConfig
                        , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                        , options = args.options
                        , selected = args.options
                        }
            in
            ( args.options
            , Model model_
            , cmd_
            )

        Preset.Listbox.Internal.MenuClosed ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.closed
                        { config = menuConfig
                        , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                        }
            in
            ( args.options
            , Model model_
            , cmd_
            )

        Preset.Listbox.Internal.MenuFocussed focussed_ ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.focussed
                        { msg = focussed_
                        , state = model
                        , config = menuConfig
                        , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                        , options = args.options
                        }
            in
            ( args.options
            , Model model_
            , cmd_
            )

        Preset.Listbox.Internal.MenuSelected selected ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Listbox.selected
                        { msg = selected
                        , state = model
                        , config = menuConfig
                        , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                        , options = args.options
                        , selected = args.options
                        }
            in
            ( selected_
            , Model model_
            , cmd_
            )

        Preset.Listbox.Internal.MenuInputted inputted ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.inputted
                        { msg = inputted
                        , state = model
                        , config = menuConfig
                        , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                        , options = args.options
                        }
            in
            ( args.options
            , Model model_
            , cmd_
            )

        Preset.Listbox.Internal.NoOp ->
            ( args.options
            , args.model
            , Cmd.none
            )


{-| Created automatically when you call [#view](#view), pass it in to view functions
-}
type Token option
    = Token (Menus.Listbox.Token (ZipList.ZipList option) option Int (ZipList.ZipList option) Msg)


{-| Check whether the menu is open or closed
-}
isOpen : Token option -> Bool
isOpen (Token token) =
    case token.state of
        Menus.Listbox.Opened _ _ ->
            True

        Menus.Listbox.Closed ->
            False


{-| Which option is focussed (returns an index)
-}
focussedOption : Token option -> Maybe Int
focussedOption (Token token) =
    token.focussed


{-| This needs to wrap your menu. It does not render any HTML, but provides a [#Token](#Token) that you pass into view functions, and also the current options.
-}
view : { model : Model, config : Config option, options : ZipList.ZipList option } -> (Token option -> ZipList.ZipList option -> Html.Html Msg) -> Html.Html Msg
view args func =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config

        token : Token option
        token =
            Token
                (Menus.Listbox.menuToken
                    { state = model
                    , config = menuConfig
                    , msgConfig = Preset.Listbox.Internal.menuMsgConfig
                    , selected = args.options
                    }
                )
    in
    func token args.options


{-| The button used to open and close the menu
-}
button : Token option -> (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg)
button (Token token) =
    Menus.Listbox.button token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.
-}
options : Token option -> (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg)
options (Token token) =
    Menus.Listbox.options token


{-| A focusable and selectable option
-}
option : Token option -> ({ value : Int, isSelected : Bool } -> List (Html.Attribute Msg) -> List (Html.Html Never) -> Html.Html Msg)
option (Token token) =
    Menus.Listbox.option token
