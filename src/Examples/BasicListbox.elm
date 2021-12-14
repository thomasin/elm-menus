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


menuConfig : Menus.Listbox.Config Examples.MenuItem.MenuItem Int
menuConfig =
    { id = "basic-listbox"
    , toLabel = .label
    , valueToString = String.fromInt
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
            , Menus.Listbox.onOpen menuConfig menuMsgConfig
            )

        MenuClosed ->
            ( { model | menu = Menus.Listbox.closed }
            , Menus.Listbox.onClose menuConfig menuMsgConfig
            )

        MenuFocussed focussed ->
            case focussed of
                Menus.Listbox.FocussedSpecific idx ->
                    ( { model | menu = Menus.Listbox.focusOn model.menu (Just idx) }
                    , Cmd.none
                    )

                Menus.Listbox.FocussedPrevious ->
                    case Menus.Listbox.currentlyFocussed model.menu of
                        Just prevIdx ->
                            ( { model | menu = Menus.Listbox.changeFocus model.menu (max (prevIdx - 1) 0) (ZipList.length model.options - 1) }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Menus.Listbox.FocussedNext ->
                    case Menus.Listbox.currentlyFocussed model.menu of
                        Just prevIdx ->
                            ( { model | menu = Menus.Listbox.changeFocus model.menu (min (prevIdx + 1) (ZipList.length model.options - 1)) 0 }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Menus.Listbox.FocusLost ->
                    ( { model | menu = Menus.Listbox.focusOn model.menu Nothing }
                    , Cmd.none
                    )

        MenuSelected selected ->
            let
                select : Int -> ZipList.ZipList Examples.MenuItem.MenuItem
                select idx =
                    ZipList.goToIndex idx model.options
                        |> Maybe.withDefault model.options
            in
            case selected of
                Menus.Listbox.SelectedSpecific idx ->
                    ( { model | options = select idx, menu = Menus.Listbox.closed }
                    , Menus.Listbox.onClose menuConfig menuMsgConfig
                    )

                Menus.Listbox.SelectedPrevious ->
                    ( { model | options = ZipList.backward model.options }
                    , Cmd.none
                    )

                Menus.Listbox.SelectedNext ->
                    ( { model | options = ZipList.forward model.options }
                    , Cmd.none
                    )

                Menus.Listbox.SelectedFocussed ->
                    case Menus.Listbox.currentlyFocussed model.menu of
                        Just focus ->
                            ( { model | options = select focus, menu = Menus.Listbox.closed }
                            , Menus.Listbox.onClose menuConfig menuMsgConfig
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

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
                [ Html.text (menuConfig.toLabel option) ]

    in
    Html.div
        [ Attr.class "box-content relative w-72 text-left select-none" ]
        [ Menus.Listbox.button model.menu
            menuConfig
            menuMsgConfig
            { classes = "cursor-default w-full bg-transparent border-b border-gray-300 focus:border-saint-patrick-blue focus:outline-none flex items-baseline justify-between px-1.5 py-2", classList = [] }
            [ Html.span
                [ Attr.class "text-sm leading-none" ]
                [ Html.text (menuConfig.toLabel (ZipList.current model.options)) ]
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
