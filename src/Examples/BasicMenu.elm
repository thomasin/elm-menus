module Examples.BasicMenu exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr
import ZipList

import Examples.MenuItem
import Menus.Menu


--


type alias Model =
    { menu : Menus.Menu.State Int
    , options : ZipList.ZipList Examples.MenuItem.MenuItem
    }


type Msg
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Menu.Focussed Int)
    | NoOp


menuConfig : Menus.Menu.Config Examples.MenuItem.MenuItem Int
menuConfig =
    { id = "basic-listbox"
    , toLabel = .label
    , valueToString = String.fromInt
    }


menuMsgConfig : Menus.Menu.MsgConfig Int Msg
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onNoOp = NoOp
    }


init : ( Model, Cmd Msg )
init =
    (
        { menu = Menus.Menu.closed
        , options =
            ZipList.new
                (Tuple.first Examples.MenuItem.nonEmpty)
                (Tuple.second Examples.MenuItem.nonEmpty)
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuOpened ->
            ( { model | menu = Menus.Menu.opened model.menu 0 }
            , Menus.Menu.onOpen menuConfig menuMsgConfig
            )

        MenuClosed ->
            ( { model | menu = Menus.Menu.closed }
            , Menus.Menu.onClose menuConfig menuMsgConfig
            )

        MenuFocussed focussed ->
            case focussed of
                Menus.Menu.FocussedSpecific idx ->
                    ( { model | menu = Menus.Menu.focusOn model.menu (Just idx) }
                    , Cmd.none
                    )

                Menus.Menu.FocussedPrevious ->
                    case Menus.Menu.currentlyFocussed model.menu of
                        Just prevIdx ->
                            ( { model | menu = Menus.Menu.changeFocus model.menu (max (prevIdx - 1) 0) (ZipList.length model.options - 1) }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Menus.Menu.FocussedNext ->
                    case Menus.Menu.currentlyFocussed model.menu of
                        Just prevIdx ->
                            ( { model | menu = Menus.Menu.changeFocus model.menu (min (prevIdx + 1) (ZipList.length model.options - 1)) 0 }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Menus.Menu.FocusLost ->
                    ( { model | menu = Menus.Menu.focusOn model.menu Nothing }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


view model =
    let
        viewOption : Int -> Examples.MenuItem.MenuItem -> Html.Html Msg
        viewOption idx option =
            Menus.Menu.li menuConfig menuMsgConfig
                idx
                { classes = "cursor-default first:rounded-t-md last:rounded-b-md border-b border-gray-100 last:border-0 px-3 py-2 first:pt-3.5 last:pb-3.5 transition-colors"
                , classList =
                    [ ( "bg-blue-100", Just idx == Menus.Menu.currentlyFocussed model.menu )
                    ]
                }
                [ Html.text (menuConfig.toLabel option) ]

    in
    Html.div
        [ Attr.class "box-content relative w-72 text-left select-none" ]
        [ Menus.Menu.button model.menu
            menuConfig
            menuMsgConfig
            { classes = "cursor-default w-full bg-transparent rounded border border-gray-200 bg-gray-100 focus:border-blue-100 focus:outline-none flex items-baseline justify-between px-2 py-3", classList = [] }
            [ Html.span
                [ Attr.class "text-sm leading-none" ]
                [ Html.text "Basic menu" ]
            ]
        , Menus.Menu.ul model.menu
            menuConfig
            menuMsgConfig
            { classes = "absolute top-full left-0 min-w-full shadow-md rounded-md outline-none focus:outline-none bg-white text-sm text-gray-900 transition-all"
            , classList =
                [ ( "z-50 opacity-100 pointer-events-all", Menus.Menu.isOpen model.menu )
                , ( "z-20 opacity-0 pointer-events-none", not (Menus.Menu.isOpen model.menu) )
                ]
            }
            (ZipList.toList model.options
                |> List.indexedMap viewOption
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.Menu.subscriptions model.menu menuMsgConfig
