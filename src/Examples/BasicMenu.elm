module Examples.BasicMenu exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr
import ZipList

import Examples.MenuItem
import Menus.Menu


--


type alias Model =
    { menu : Menus.Menu.State
    }


type Msg
    = MenuOpened
    | MenuClosed
    | MenuFocussed Menus.Menu.Focussed
    | NoOp


menuConfig : Menus.Menu.Config (List Examples.MenuItem.MenuItem)
menuConfig =
    { id = "basic-listbox"
    , optionsLength = List.length
    }


menuMsgConfig : Menus.Menu.MsgConfig Msg
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onNoOp = NoOp
    }


init : ( Model, Cmd Msg )
init =
    ( { menu = Menus.Menu.closed }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuOpened ->
            ( { model | menu = Menus.Menu.opened model.menu 0 }
            , Cmd.none
            )

        MenuClosed ->
            ( { model | menu = Menus.Menu.closed }
            , Cmd.none
            )

        MenuFocussed focussed ->
            let
                ( model_, cmd_ ) =
                    Menus.Menu.focussed
                        { msg = focussed
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = Examples.MenuItem.list
                        }
            in
            ( { model | menu = model_ }
            , cmd_
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
                [ Html.text option.label ]

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
            ( List.indexedMap viewOption Examples.MenuItem.list
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.Menu.subscriptions model.menu menuMsgConfig
