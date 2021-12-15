module Examples.BasicListbox exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr
import ZipList

import Examples.MenuItem
import Menus.Listbox


--


type alias Model =
    { menu : Menus.Listbox.State Int
    , options : ZipList.ZipList Examples.MenuItem.MenuItem
    }


type Msg
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Listbox.Focussed Int)
    | MenuSelected (Menus.Listbox.Selected Int)
    | NoOp


menuConfig : Menus.Listbox.Config (Menus.Listbox.Options Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Int (Menus.Listbox.Options Examples.MenuItem.MenuItem)
menuConfig =
    Menus.Listbox.ziplist
        { id = "basic-listbox"
        , optionToLabel = .label
        }


menuMsgConfig : Menus.Listbox.MsgConfig Int Msg
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onSelected = MenuSelected
    , onInputted = always NoOp
    , onNoOp = NoOp
    }


init : ( Model, Cmd Msg )
init =
    (
        { menu = Menus.Listbox.closed
        , options = ZipList.new (Tuple.first Examples.MenuItem.nonEmpty) (Tuple.second Examples.MenuItem.nonEmpty)
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuOpened ->
            ( { model | menu = Menus.Listbox.opened model.menu 0 }
            , Cmd.none
            )

        MenuClosed ->
            ( { model | menu = Menus.Listbox.closed }
            , Cmd.none
            )

        MenuFocussed focussed ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.focussed
                        { msg = focussed
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = model.options
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuSelected selected ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Listbox.selected
                        { msg = selected
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = model.options
                        , selected = model.options
                        }
            in
            ( { model | menu = model_, options = selected_ }
            , cmd_
            )

        --MenuInputted inputted ->
        --    let
        --        ( selected_, model_, cmd_ ) =
        --            Menus.Listbox.inputted
        --                { msg = inputted
        --                , state = model.menu
        --                , config = menuConfig
        --                , msgConfig = menuMsgConfig
        --                , options = Examples.MenuItem.list
        --                , selected = model.selected
        --                }
        --    in
        --    ( { model | menu = model_, selected = selected_ }
        --    , cmd_
        --    )

        NoOp ->
            ( model, Cmd.none )


view model =
    let
        viewOption : Int -> Bool -> Examples.MenuItem.MenuItem -> Html.Html Msg
        viewOption idx isSelected option =
            Menus.Listbox.option menuConfig menuMsgConfig
                idx
                isSelected
                { classes = "cursor-default first:rounded-t-md last:rounded-b-md px-3 py-2 first:pt-3.5 last:pb-3.5 transition-colors"
                , classList =
                    [ ( "bg-blue-100", Just idx == Menus.Listbox.currentlyFocussed model.menu )
                    , ( "text-blue-900", isSelected )
                    ]
                }
                [ Html.text (menuConfig.optionToLabel option) ]

    in
    Html.div
        [ Attr.class "box-content relative w-72 text-left select-none" ]
        [ Menus.Listbox.button model.menu
            menuConfig
            menuMsgConfig
            { classes = "cursor-default w-full bg-transparent border-b border-gray-300 focus:border-saint-patrick-blue focus:outline-none flex items-baseline justify-between px-1.5 py-2", classList = [] }
            [ Html.span
                [ Attr.class "text-sm leading-none" ]
                [ Html.text (menuConfig.optionToLabel (ZipList.current model.options)) ]
            ]
        , Menus.Listbox.options model.menu
            menuConfig
            menuMsgConfig
            (Just (ZipList.currentIndex model.options))
            { classes = "absolute top-0 left-0 min-w-full shadow-md rounded-md outline-none focus:outline-none bg-white text-sm text-gray-900 transition-all"
            , classList =
                [ ( "z-50 opacity-100 pointer-events-all", Menus.Listbox.isOpen model.menu )
                , ( "z-20 opacity-0 pointer-events-none", not (Menus.Listbox.isOpen model.menu) )
                ]
            }
            (ZipList.indexedSelectedMap (viewOption) model.options
                |> ZipList.toList
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.Listbox.subscriptions model.menu menuMsgConfig
