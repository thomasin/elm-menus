module Preset.Listbox exposing (Model, Msg, OptionConfig, Config, init, update, Token, view, config, options, button, option, isOpen, focussedOption)

{-| A custom <select>, with support for keyboard navigation
This is an attempt to reimplement the native <select> element with custom
styling. This a basic first step, that doesn't include support for multi-select,
always open listboxes, or grouped options.

@docs Model, Msg, OptionConfig, Config, init, update, Token, view, config, options, button, option, isOpen, focussedOption

-}

import Html
import Menus.Focus
import Menus.Listbox
import Menus.Select
import Menus.Select.Internal
import Preset.Listbox.Internal
import ZipList



--


{-| Store this in your model
-}
type Model
    = Model (Menus.Listbox.State Int)


{-| Create in [#config](#config)
-}
type Config option
    = Config (Menus.Listbox.Config (ZipList.ZipList option) option Int (ZipList.ZipList option))


{-| Convenience alias for Msg, passed in to [#update](#update)
-}
type alias Msg =
    Preset.Listbox.Internal.Msg


{-| Pass in to [#config](#config)
-}
type alias OptionConfig option =
    { id : String
    , optionToLabel : option -> String
    }


{-| Pass in an id to uniquely identity this dropdown.
It _must_ be unique to the page or weird things will happen.
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


{-| Initialise a closed listbox
-}
init : Model
init =
    Model Menus.Listbox.init


{-| Call this in your update function
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


{-| Passed in to view functions
-}
type Token option
    = Token (Menus.Listbox.Token (ZipList.ZipList option) option Int (ZipList.ZipList option) Msg)


{-| Whether the menu is open or closed
-}
isOpen : Token option -> Bool
isOpen (Token token) =
    case token.state of
        Menus.Listbox.Opened _ _ ->
            True

        Menus.Listbox.Closed ->
            False


{-| Which item is focussed (returns an index)
-}
focussedOption : Token option -> Maybe Int
focussedOption (Token token) =
    token.focussed


{-| Generates a token for you
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


{-| A focus and selectable option
-}
option : Token option -> ({ value : Int, isSelected : Bool } -> List (Html.Attribute Msg) -> List (Html.Html Never) -> Html.Html Msg)
option (Token token) =
    Menus.Listbox.option token
