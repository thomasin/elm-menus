module Examples.MultiSelectCombobox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Html.Events
import List.Extra as List
import Menus.Combobox
import Preset.Combobox
import Menus.Select
import Menus.Focus
import Menus.Active



--


type alias Model =
    { menu : Menus.Combobox.State Examples.MenuItem.MenuItem
    , selected : List Examples.MenuItem.MenuItem
    , options : List Examples.MenuItem.MenuItem
    , justOpened : Bool
    }


type Msg
    = MenuMsg (Preset.Combobox.Msg Examples.MenuItem.MenuItem)
    | SelectionRemoved Examples.MenuItem.MenuItem
    | LastSelectionRemoved


menuConfig : Bool -> Menus.Combobox.Config (List Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Examples.MenuItem.MenuItem (List Examples.MenuItem.MenuItem)
menuConfig justOpened =
    let
        optionConfig =
            { id = "multiselect-combobox"
            , optionToLabel = .label
            , optionToId = String.fromInt << .id
            , matchesInput = \str option -> String.toLower str == String.toLower option.label
            , containsInput = \str option -> String.startsWith (String.toLower str) (String.toLower option.label)
            , visibleOptions = \config str selected options -> options
            }

    in
    Menus.Combobox.emptiable
        { id = optionConfig.id
        , empty = []
        , optionToLabel = optionConfig.optionToLabel
        , optionToValue = identity
        , valueToString = optionConfig.optionToId
        , selectionToLabel = \_ ->
            ""
        , selectionToOption =
            \selected ->
                List.head selected
        , selectChange =
            \action selected _ ->
                case action of
                    Menus.Select.MovedLeft ->
                        Menus.Select.NotChanged

                    Menus.Select.MovedRight ->
                        Menus.Select.NotChanged

                    Menus.Select.Cleared ->
                        Menus.Select.ChangedTo (List.drop 1 selected)
        , selectValue =
            \value selected _ ->
                value :: selected
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
                    unselectedOpts : List Examples.MenuItem.MenuItem
                    unselectedOpts =
                        List.filter (\opt -> not (List.member opt selected)) opts

                in
                if justOpened || str == "" then
                    case List.filter (optionConfig.matchesInput str) unselectedOpts of
                        exactMatch :: _ ->
                            Just ( Menus.Active.Focussed exactMatch, unselectedOpts )

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


init : ( Model, Cmd Msg )
init =
    ( { menu = Menus.Combobox.init
      , selected = []
      , options = Examples.MenuItem.list
      , justOpened = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectionRemoved option ->
            ( { model | selected = List.filter ((/=) option) model.selected }
            , Cmd.none
            )

        LastSelectionRemoved ->
            ( { model | selected = List.drop 1 model.selected }
            , Cmd.none
            )

        MenuMsg Preset.Combobox.MenuOpened ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.opened
                        { state = model.menu
                        , config = (menuConfig True)
                        , msgConfig = Preset.Combobox.menuMsgConfig
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, justOpened = True }
            , Cmd.map MenuMsg cmd_
            )

        MenuMsg Preset.Combobox.MenuClosed ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.closed
                        { config = (menuConfig model.justOpened)
                        , msgConfig = Preset.Combobox.menuMsgConfig
                        }
            in
            ( { model | menu = model_ }
            , Cmd.map MenuMsg cmd_
            )

        MenuMsg (Preset.Combobox.MenuFocussed focussed) ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.focussed
                        { msg = focussed
                        , state = model.menu
                        , config = (menuConfig model.justOpened)
                        , msgConfig = Preset.Combobox.menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_ }
            , Cmd.map MenuMsg cmd_
            )

        MenuMsg (Preset.Combobox.MenuSelected selected) ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.selected
                        { msg = selected
                        , state = model.menu
                        , config = (menuConfig model.justOpened)
                        , msgConfig = Preset.Combobox.menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , Cmd.map MenuMsg cmd_
            )

        MenuMsg (Preset.Combobox.MenuInputted inputted) ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = (menuConfig False)
                        , msgConfig = Preset.Combobox.menuMsgConfig
                        , options = model.options
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_, justOpened = False }
            , Cmd.map MenuMsg cmd_
            )

        MenuMsg Preset.Combobox.NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        token : Menus.Combobox.Token (List Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Examples.MenuItem.MenuItem (List Examples.MenuItem.MenuItem) (Preset.Combobox.Msg Examples.MenuItem.MenuItem)
        token =
            Menus.Combobox.menuToken
                { state = model.menu
                , config = menuConfig model.justOpened
                , msgConfig = Preset.Combobox.menuMsgConfig
                , selected = model.selected
                }

        visibleOptions : List Examples.MenuItem.MenuItem
        visibleOptions =
            Maybe.withDefault []
                (Menus.Combobox.visibleOptions
                    { state = model.menu
                    , config = menuConfig model.justOpened
                    , selected = model.selected
                    , options = model.options
                    }
                )

    in
    Html.div
        [ Attr.class "relative w-96 min-h-[44px] flex flex-wrap items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1 px-1 py-2"
        ]
        [ Html.div
            [ Attr.class "flex flex-wrap gap-y-2 shrink-1" ]
            (
                (List.map
                    (\option ->
                        Html.div
                            [ Attr.class "text-xs bg-stone-100 border rounded mx-1 last:-mr-1 flex" ]
                            [ Html.span
                                [ Attr.class "py-1 pl-2" ]
                                [ Html.text option.label
                                ]
                            , Html.button
                                [ Attr.class "ml-1.5 hover:bg-red-400 rounded-r scale-100 hover:scale-125 px-2 transition"
                                , Html.Events.onClick (SelectionRemoved option)
                                ]
                                [ Html.text "X"
                                ]
                            ]
                    )
                    (List.reverse model.selected)
                )
            ++ [Html.map MenuMsg
            (Menus.Combobox.input token
                { placeholder = "Select" }
                [ Attr.class "grow-1 self-stretch rounded bg-transparent flex items-center text-sm placeholder-stone-400"
                , Attr.classList
                    [ ( "pl-2.5 pr-3.5", List.length model.selected > 0 )
                    , ( "pl-2.5 pr-3.5", List.length model.selected == 0 )
                    ]
                
                ]
            )])
        , Html.button
            [ Attr.class "absolute right-2 w-5 flex flex-col text-purple-900"
            , Html.Events.onMouseDown
                (if Menus.Combobox.isOpen model.menu then
                    MenuMsg Preset.Combobox.menuClosed

                else
                    MenuMsg Preset.Combobox.menuOpened
                )
            ]
            [ if Menus.Combobox.isOpen model.menu then
                Examples.Svg.chevronUp

            else
                Examples.Svg.chevronDown
            ]
        , Html.map MenuMsg
            (Menus.Combobox.options token
                [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md will-change rounded outline-none border border-stone-300 p-1 pt-0 bg-white text-sm text-stone-900"
                , Attr.classList -- max-h-48 overflow-y-scroll shadow-md
                    [ ( "z-50 opacity-100 pointer-events-all", Menus.Combobox.isOpen model.menu && List.length visibleOptions > 0 )
                    , ( "z-0 opacity-0 pointer-events-none", not (Menus.Combobox.isOpen model.menu) || List.length visibleOptions == 0 )
                    ]
                ]
                (List.map
                    (\option ->
                        Menus.Combobox.option token
                            { value = option, isSelected = List.member option model.selected }
                            [ Attr.class "pt-1" ]
                            [ Html.div
                                [ Attr.class "flex cursor-default rounded px-3.5 py-3"
                                , Attr.classList
                                    [ ( "bg-purple-100", Just option == token.focussed )
                                    , ( "bg-transparent", not (Just option == token.focussed) )
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.class "w-5 flex justify-center mr-2 text-purple-900"
                                    ]
                                    [ if List.member option model.selected then
                                        Examples.Svg.check

                                    else
                                        Html.text option.emoji
                                    ]
                                , Html.text option.label
                                ]
                            ]
                    )
                    visibleOptions
                )
            )
        ]
