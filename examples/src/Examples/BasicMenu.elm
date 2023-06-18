module Examples.BasicMenu exposing (Model, Msg, init, update, view)

import Accessibility.Key as Key
import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Preset.Menu



--


type alias Model =
    Preset.Menu.Model


type alias Msg =
    Preset.Menu.Msg


menuConfig : Preset.Menu.Config Examples.MenuItem.MenuItem
menuConfig =
    Preset.Menu.config "basic-menu"


init : ( Model, Cmd Msg )
init =
    ( Preset.Menu.init
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Preset.Menu.update msg
        { model = model
        , config = menuConfig
        , options = Examples.MenuItem.list
        }


view : Model -> Html.Html Msg
view model =
    Preset.Menu.view
        { model = model
        , config = menuConfig
        }
        (\token ->
            Html.div
                [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between relative h-11 flex items-center border border-stone-300 bg-transparent rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
                ]
                [ Preset.Menu.button token
                    [ Attr.class "cursor-default h-full w-full flex items-center leading-none justify-between px-3.5 text-sm text-stone-900"
                    ]
                    [ Html.text "Examples"
                    ]
                , Html.button
                    [ Attr.class "absolute cursor-pointer right-2 w-5 text-purple-900 pointer-events-none"
                    , Key.tabbable False
                    ]
                    [ if Preset.Menu.isOpen token then
                        Examples.Svg.chevronUp

                      else
                        Examples.Svg.chevronDown
                    ]
                , Preset.Menu.options token
                    [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md rounded border border-stone-300 p-1 space-y-1 bg-white text-sm text-stone-900"
                    , Attr.classList
                        [ ( "z-50 opacity-100 pointer-events-all", Preset.Menu.isOpen token )
                        , ( "z-0 opacity-0 pointer-events-none", not (Preset.Menu.isOpen token) )
                        ]
                    ]
                    (List.indexedMap
                        (\idx option ->
                            Preset.Menu.link token
                                { idx = idx }
                                []
                                (\link ->
                                    link
                                        [ Attr.href "/"
                                        , Attr.class "block w-full cursor-pointer rounded px-3 py-2 bg-transparent focus:bg-purple-100"
                                        ]
                                        [ Html.text option.label ]
                                )
                        )
                        Examples.MenuItem.list
                    )
                ]
        )
