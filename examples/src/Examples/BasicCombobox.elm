module Examples.BasicCombobox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Html.Events
import Menus.Select
import Preset.Combobox



--


type alias Model =
    { menu : Preset.Combobox.Model Examples.MenuItem.MenuItem
    , options : List Examples.MenuItem.MenuItem
    , selected : Maybe Examples.MenuItem.MenuItem
    }


type alias Msg =
    Preset.Combobox.Msg Examples.MenuItem.MenuItem


menuConfig : Preset.Combobox.Context -> Preset.Combobox.Config Examples.MenuItem.MenuItem (Maybe Examples.MenuItem.MenuItem)
menuConfig =
    Preset.Combobox.config
        { id = "basic-combobox"
        , optionToLabel = .label
        , optionToId = String.fromInt << .id
        , optionToSelection = Just
        , selectionToLabel = Maybe.withDefault "" << Maybe.map .label
        , selectionToOption = \selected _ -> selected
        , selectionCleared = Menus.Select.ChangedTo Nothing
        , matchesInput = \str option -> String.toLower str == String.toLower option.label
        , containsInput = \str option -> String.contains (String.toLower str) (String.toLower option.label)
        , visibleOptions = \_ _ _ options -> options
        }


init : ( Model, Cmd Msg )
init =
    ( { menu = Preset.Combobox.init
      , options = Examples.MenuItem.list
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
                    [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md will-change rounded outline-none border border-stone-300 p-1 pt-0 bg-white text-sm text-stone-900"
                    , Attr.classList
                        -- max-h-48 overflow-y-scroll shadow-md
                        [ ( "z-50 opacity-100 pointer-events-all", Preset.Combobox.isOpen menu )
                        , ( "z-0 opacity-0 pointer-events-none", not (Preset.Combobox.isOpen menu) )
                        ]
                    ]
                    (case visibleOptions of
                        [] ->
                            [ Html.text "No matching options"
                            ]

                        _ ->
                            List.map
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
                                                [ Attr.class "w-5 flex justify-center mr-2 text-purple-900"
                                                ]
                                                [ if Just option == model.selected then
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
