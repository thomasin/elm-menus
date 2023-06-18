module Examples.MultiSelectCombobox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Html.Events
import Preset.MultiCombobox



--


type alias Model =
    { menu : Preset.MultiCombobox.Model Examples.MenuItem.MenuItem
    , options : List Examples.MenuItem.MenuItem
    , selected : List Examples.MenuItem.MenuItem
    }


type alias Msg =
    Preset.MultiCombobox.Msg Examples.MenuItem.MenuItem


type alias Config =
    Preset.MultiCombobox.Config Examples.MenuItem.MenuItem


menuConfig : Preset.MultiCombobox.Context -> Config
menuConfig =
    Preset.MultiCombobox.config
        { id = "multiselect-combobox"
        , optionToLabel = .label
        , optionToId = String.fromInt << .id
        , matchesInput = \str option -> String.toLower str == String.toLower option.label
        , containsInput = \str option -> String.startsWith (String.toLower str) (String.toLower option.label)
        , visibleOptions = \_ _ _ options -> options
        }


init : ( Model, Cmd Msg )
init =
    ( { menu = Preset.MultiCombobox.init
      , options = Examples.MenuItem.list
      , selected = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( selected, menu, cmd ) =
            Preset.MultiCombobox.update msg
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
    Preset.MultiCombobox.view
        { model = model.menu
        , config = menuConfig
        , options = model.options
        , selected = model.selected
        }
        (\menu visibleOptions ->
            Html.div
                [ Attr.class "relative w-96 min-h-[44px] flex flex-wrap items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1 px-1 py-2"
                ]
                [ Html.div
                    [ Attr.class "flex flex-wrap gap-y-2 shrink-1" ]
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
                                    , Html.Events.onClick (Preset.MultiCombobox.selectionRemoved option)
                                    ]
                                    [ Html.text "X"
                                    ]
                                ]
                        )
                        (List.reverse model.selected)
                        ++ [ Preset.MultiCombobox.input menu
                                { placeholder = "Select" }
                                [ Attr.class "grow-1 self-stretch rounded bg-transparent flex items-center text-sm placeholder-stone-400"
                                , Attr.classList
                                    [ ( "pl-2.5 pr-3.5", List.length model.selected > 0 )
                                    , ( "pl-2.5 pr-3.5", List.length model.selected == 0 )
                                    ]
                                ]
                           ]
                    )
                , Html.button
                    [ Attr.class "absolute right-2 w-5 flex flex-col text-purple-900"
                    , Html.Events.onMouseDown
                        (if Preset.MultiCombobox.isOpen menu then
                            Preset.MultiCombobox.menuClosed

                         else
                            Preset.MultiCombobox.menuOpened
                        )
                    ]
                    [ if Preset.MultiCombobox.isOpen menu then
                        Examples.Svg.chevronUp

                      else
                        Examples.Svg.chevronDown
                    ]
                , Preset.MultiCombobox.options menu
                    [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md will-change rounded outline-none border border-stone-300 p-1 pt-0 bg-white text-sm text-stone-900"
                    , Attr.classList
                        -- max-h-48 overflow-y-scroll shadow-md
                        [ ( "z-50 opacity-100 pointer-events-all", Preset.MultiCombobox.isOpen menu && List.length visibleOptions > 0 )
                        , ( "z-0 opacity-0 pointer-events-none", not (Preset.MultiCombobox.isOpen menu) || List.length visibleOptions == 0 )
                        ]
                    ]
                    (List.map
                        (\option ->
                            Preset.MultiCombobox.option menu
                                { value = option, isSelected = List.member option model.selected }
                                [ Attr.class "pt-1" ]
                                [ Html.div
                                    [ Attr.class "flex cursor-default rounded px-3.5 py-3"
                                    , Attr.classList
                                        [ ( "bg-purple-100", Just option == Preset.MultiCombobox.focussedOption menu )
                                        , ( "bg-transparent", not (Just option == Preset.MultiCombobox.focussedOption menu) )
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
                ]
        )
