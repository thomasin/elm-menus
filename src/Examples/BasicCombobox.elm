module Examples.BasicCombobox exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr

import Examples.MenuItem
import Menus.Combobox


--


type alias Model =
    { menu : Menus.Combobox.State Examples.MenuItem.MenuItem
    , selected : Maybe Examples.MenuItem.MenuItem
    , options : List Examples.MenuItem.MenuItem
    }


type Msg
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Combobox.Focussed Examples.MenuItem.MenuItem)
    | MenuSelected (Menus.Combobox.Selected Examples.MenuItem.MenuItem)
    | MenuInputted Menus.Combobox.Inputted
    | NoOp


menuConfig : Menus.Combobox.Config (List Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Examples.MenuItem.MenuItem (Maybe Examples.MenuItem.MenuItem)
menuConfig =
    Menus.Combobox.listbox
        { id = "basic-combobox"
        , optionToLabel = .label
        , optionToValue = identity
        , valueToString = String.fromInt << .id
        }


menuMsgConfig : Menus.Combobox.MsgConfig Examples.MenuItem.MenuItem Msg
menuMsgConfig =
    { onOpened = MenuOpened
    , onClosed = MenuClosed
    , onFocussed = MenuFocussed
    , onSelected = MenuSelected
    , onInput = MenuInputted
    , onNoOp = NoOp
    }


init : ( Model, Cmd Msg )
init =
    (
        { menu = Menus.Combobox.init
        , selected = Nothing
        , options = Examples.MenuItem.list
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuOpened ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.opened
                        { state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuClosed ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.closed
                        { config = menuConfig
                        , msgConfig = menuMsgConfig
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuFocussed focussed ->
            let
                ( model_, cmd_ ) =
                    Menus.Combobox.focussed
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

        MenuSelected selected ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.selected
                        { msg = selected
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = Examples.MenuItem.list
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        MenuInputted inputted ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.Combobox.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = Examples.MenuItem.list
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        NoOp ->
            ( model, Cmd.none )


view model =
    let
        isOpen =
            Menus.Combobox.isOpen model.menu

        focussed =
            Menus.Combobox.currentlyFocussed model.menu

        ( token, input ) = Menus.Combobox.tokenised Menus.Combobox.input
            model.menu
            menuConfig
            menuMsgConfig
            model.selected

        viewOption option =
            Menus.Combobox.option token
                option
                (Just option == model.selected)
                { classes = "flex cursor-default last:rounded-b-md last:pb-3.5 focus:outline-none px-3.5 py-3 transition-colors"
                , classList =
                    [ ( "text-blue-900 font-semibold", Just option == model.selected )
                    , ( "bg-blue-100", Just option == focussed )
                    ]
                }
                [ Html.div
                    [ Attr.class "mr-2" ]
                    [ Html.text (if Just option == model.selected then "✅" else option.emoji) ]
                , Html.text option.label
                ]
    in
    Menus.Combobox.container menuMsgConfig
        { classes = "relative w-72 flex flex-wrap justify-between" }
        [ Html.div
            [ Attr.class "relative w-full h-11 flex items-center border border-gray-300 bg-white rounded shadow-sm"
            , Attr.classList
                [ ( "rounded-b-none border-b-transparent", isOpen )
                ]
            ]
            [ input
                (Maybe.map menuConfig.optionToLabel model.selected)
                { placeholder = "Select"
                , classes = "w-full h-full rounded focus:outline-none focus:ring focus:ring-blue-100 focus:ring-offset-gray-50 focus:ring-offset-2 pl-3.5 bg-transparent flex items-center text-sm text-saint-patrick-blue placeholder-silver-seand transition-colors"
                , classList = []
                }
            ]
        , Menus.Combobox.options token
            { classes = "absolute top-full left-0 min-w-full max-h-48 overflow-y-scroll border border-gray-300 shadow-sm rounded-b-md outline-none focus:outline-none bg-white text-sm text-gray-900 leading-none"
            , classList =
                [ ( "opacity-100 pointer-events-all", isOpen )
                , ( "opacity-0 pointer-events-none", not isOpen )
                ]
            }
            (case isOpen of
                True ->
                    List.map viewOption model.options

                False ->
                    []
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.Combobox.subscriptions model.menu menuMsgConfig
