module Examples.BasicListbox exposing (Model, Msg, init, update, view)

import Accessibility.Key as Key
import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Preset.Listbox
import ZipList



--


type alias Model =
    { menu : Preset.Listbox.Model
    , options : ZipList.ZipList Examples.MenuItem.MenuItem
    }


type alias Msg =
    Preset.Listbox.Msg


menuConfig : Preset.Listbox.Config Examples.MenuItem.MenuItem
menuConfig =
    Preset.Listbox.config
        { id = "basic-listbox"
        , optionToLabel = .label
        }


init : ( Model, Cmd Msg )
init =
    ( { menu = Preset.Listbox.init
      , options = ZipList.new (Tuple.first Examples.MenuItem.nonEmpty) (Tuple.second Examples.MenuItem.nonEmpty)
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( options, menu, cmd ) =
            Preset.Listbox.update msg
                { model = model.menu
                , config = menuConfig
                , options = model.options
                }
    in
    ( { model | menu = menu, options = options }
    , cmd
    )


view : Model -> Html.Html Msg
view model =
    Preset.Listbox.view
        { model = model.menu
        , config = menuConfig
        , options = model.options
        }
        (\menu options ->
            Html.div
                [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between relative h-11 flex items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
                ]
                [ Preset.Listbox.button menu
                    [ Attr.class "w-full h-full rounded bg-transparent flex items-center px-3.5 text-sm" ]
                    [ Html.text (.emoji (ZipList.current options))
                    , Html.span
                        [ Attr.class "ml-2" ]
                        [ Html.text (.label (ZipList.current options))
                        ]
                    ]
                , Html.button
                    [ Attr.class "absolute cursor-pointer right-2 w-5 pointer-events-none text-purple-900"
                    , Key.tabbable False
                    ]
                    [ if Preset.Listbox.isOpen menu then
                        Examples.Svg.chevronUp

                      else
                        Examples.Svg.chevronDown
                    ]
                , Preset.Listbox.options menu
                    [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md rounded border border-stone-300 p-1 space-y-1 bg-white text-sm text-stone-900"
                    , Attr.classList
                        [ ( "z-50 opacity-100 pointer-events-all", Preset.Listbox.isOpen menu )
                        , ( "z-0 opacity-0 pointer-events-none", not (Preset.Listbox.isOpen menu) )
                        ]
                    ]
                    (ZipList.toList
                        (ZipList.indexedSelectedMap
                            (\idx isSelected option ->
                                Preset.Listbox.option menu
                                    { value = idx, isSelected = isSelected }
                                    [ Attr.class "cursor-default rounded flex px-3.5 py-3"
                                    , Attr.classList
                                        [ ( "bg-purple-100", Just idx == Preset.Listbox.focussedOption menu )
                                        , ( "bg-transparent", not (Just idx == Preset.Listbox.focussedOption menu) )
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attr.class "w-5 flex justify-center mr-2 text-purple-900"
                                        ]
                                        [ if isSelected then
                                            Examples.Svg.check

                                          else
                                            Html.text option.emoji
                                        ]
                                    , Html.text option.label
                                    ]
                            )
                            options
                        )
                    )
                ]
        )
