module Examples.CreatableCombobox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Html.Events
import Menus.Select
import Preset.Combobox



--


type Option
    = Existing Examples.MenuItem.MenuItem
    | New String


type alias Model =
    { menu : Preset.Combobox.Model Option
    , options : List Option
    , selected : Maybe Option
    }


type alias Msg =
    Preset.Combobox.Msg Option


menuConfig : Preset.Combobox.Context -> Preset.Combobox.Config Option (Maybe Option)
menuConfig =
    let
        optionToLabel : Option -> String
        optionToLabel option =
            case option of
                Existing existing ->
                    existing.label

                New label ->
                    label
    in
    Preset.Combobox.config
        { id = "creatable-combobox"
        , optionToLabel = optionToLabel
        , optionToId =
            \value ->
                case value of
                    Existing option ->
                        String.fromInt option.id

                    New _ ->
                        "-1"
        , optionToSelection = Just
        , selectionToLabel = Maybe.withDefault "" << Maybe.map optionToLabel
        , selectionToOption = \selected _ -> selected
        , selectionCleared = Menus.Select.ChangedTo Nothing
        , matchesInput = \str option -> String.toLower str == String.toLower (optionToLabel option)
        , containsInput = \str option -> String.contains (String.toLower str) (String.toLower (optionToLabel option))
        , visibleOptions =
            \config str selected options ->
                Preset.Combobox.creatable { newOption = New } config str selected options
                    |> Preset.Combobox.autoSelect config str selected
        }


init : ( Model, Cmd Msg )
init =
    ( { menu = Preset.Combobox.init
      , options = List.map Existing Examples.MenuItem.list
      , selected = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( selected, menu, cmd ) =
            Preset.Combobox.update msg
                { model = model.menu
                , config = menuConfig
                , options = model.options
                , selected = model.selected
                }
    in
    ( { model | menu = menu, selected = selected }
    , cmd
    )


view : Model -> Html.Html Msg
view model =
    Preset.Combobox.view
        { model = model.menu
        , config = menuConfig
        , options = model.options
        , selected = model.selected
        }
        (\menu visibleOptions ->
            Html.div
                [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between h-11 items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
                ]
                [ Preset.Combobox.input menu
                    { placeholder = "Select" }
                    [ Attr.class "w-full h-full rounded bg-transparent flex items-center px-3.5 text-sm placeholder-stone-400" ]
                , Html.button
                    [ Attr.class "absolute right-2 w-5 flex flex-col text-purple-900"
                    , Html.Events.onMouseDown
                        (if Preset.Combobox.isOpen menu then
                            Preset.Combobox.menuClosed

                         else
                            Preset.Combobox.menuOpened
                        )
                    ]
                    [ if Preset.Combobox.isOpen menu then
                        Examples.Svg.chevronUp

                      else
                        Examples.Svg.chevronDown
                    ]
                , Preset.Combobox.options menu
                    [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md rounded outline-none border border-stone-300 p-1 pt-0 bg-white text-sm text-stone-900"
                    , Attr.classList
                        -- max-h-48 overflow-y-scroll shadow-md
                        [ ( "z-50 opacity-100 pointer-events-all", Preset.Combobox.isOpen menu )
                        , ( "z-0 opacity-0 pointer-events-none", not (Preset.Combobox.isOpen menu) )
                        ]
                    ]
                    (List.map
                        (\option ->
                            Preset.Combobox.option menu
                                { value = option, isSelected = Just option == model.selected }
                                [ Attr.class "pt-1" ]
                                [ Html.div
                                    [ Attr.class "flex cursor-default rounded px-3.5 py-3"
                                    , Attr.classList
                                        [ ( "bg-purple-100", Just option == Preset.Combobox.focussedOption menu )
                                        , ( "bg-transparent", not (Just option == Preset.Combobox.focussedOption menu) )
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attr.class "w-5 flex justify-center items-center mr-2 text-purple-900"
                                        ]
                                        [ if Just option == model.selected then
                                            Examples.Svg.check

                                          else
                                            case option of
                                                Existing opt ->
                                                    Html.text opt.emoji

                                                New _ ->
                                                    Html.span
                                                        [ Attr.class "text-xs" ]
                                                        [ Html.text "+" ]
                                        ]
                                    , case option of
                                        Existing opt ->
                                            Html.text opt.label

                                        New label ->
                                            Html.text label
                                    ]
                                ]
                        )
                        visibleOptions
                    )
                ]
        )
