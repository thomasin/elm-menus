module Examples.MultiCombobox exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import List.Extra as List

import Examples.MenuItem
import Menus.Combobox


--


type alias Model =
    { menu : Menus.Combobox.State Examples.MenuItem.MenuItem
    , selected : List Examples.MenuItem.MenuItem
    , options : List Examples.MenuItem.MenuItem
    }


type Msg
    = MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.Combobox.Focussed Examples.MenuItem.MenuItem)
    | MenuSelected (Menus.Combobox.Selected Examples.MenuItem.MenuItem)
    | MenuInputted Menus.Combobox.Inputted
    | RemoveSelected Int
    | NoOp


menuConfig : Model -> Menus.Combobox.Config (List Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Examples.MenuItem.MenuItem (List Examples.MenuItem.MenuItem)
menuConfig model =
    let
        config =
            Menus.Combobox.listbox
                { id = "multi-combobox"
                , optionToLabel = .label
                , optionToValue = identity
                , valueToString = String.fromInt << .id
                }
    in
    { id = config.id
    , optionToLabel = config.optionToLabel
    , optionToValue = config.optionToValue
    , valueToString = config.valueToString
    , selectionToOption = always Menus.Combobox.NotFound
    , selectChange = \direction selected opts -> selected
    , selectValue = \value selected opts -> List.unique (value :: selected)
    , focusChange = config.focusChange
    , focusMatch = config.focusMatch
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
        , selected = []
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
                        , config = (menuConfig model)
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
                        { config = (menuConfig model)
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
                        , config = (menuConfig model)
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
                        , config = (menuConfig model)
                        , msgConfig = menuMsgConfig
                        , options = Examples.MenuItem.list
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        --MenuInputted Menus.Combobox.BackspacePressed ->
        --    ( { model | selected = Maybe.withDefault [] (List.tail model.selected) }, Cmd.none )

        MenuInputted inputted ->
            let
                ( _, model_, cmd_ ) =
                    Menus.Combobox.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = (menuConfig model)
                        , msgConfig = menuMsgConfig
                        , options = Examples.MenuItem.list
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        RemoveSelected optionId ->
            ( { model | selected = List.filter ((/=) optionId << .id ) model.selected }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        isOpen =
            Menus.Combobox.isOpen model.menu

        viewSelected option =
            Html.div
                [ Attr.class "inline-flex items-center h-[26px] my-1 flex ml-1 text-sm"
                ]
                [ Html.div
                    [ Attr.class "py-1 pl-2.5 pr-1 bg-gray-100 rounded-l-md" ]
                    [ Html.text option.emoji ]
                , Html.button
                    [ Attr.class "py-1 pr-2.5 pl-2 border-l border-gray-50 bg-gray-100 hover:bg-red-300 text-gray-700 hover:text-red-800 font-semibold rounded-r-md"
                    , Events.onClick (RemoveSelected option.id)
                    ]
                    [ Html.text "x" ]
                ]

        ( token, control ) = Menus.Combobox.tokenised Menus.Combobox.control
            model.menu
            (menuConfig model)
            menuMsgConfig
            model.selected

        viewOption option =
            Menus.Combobox.option token
                option
                (List.member option model.selected)
                { classes = "flex cursor-default last:rounded-b-md last:pb-3.5 focus:outline-none px-3.5 py-3 transition-colors focus:bg-blue-100"
                , classList =
                    [ ( "text-blue-900 font-semibold", List.member option model.selected )
                    ]
                }
                [ Html.div
                    [ Attr.class "mr-2" ]
                    [ Html.text (if List.member option model.selected then "✅" else option.emoji) ]
                , Html.text option.label
                ]

    in
    Menus.Combobox.container menuMsgConfig
        { classes = "relative w-72 flex justify-between" }
        [ Html.div
            [ Attr.class "py-1 px-1 relative w-full min-h-[44px] flex flex-wrap items-center border border-gray-300 bg-white rounded shadow-sm"
            , Attr.classList
                [ ( "rounded-b-none border-b-transparent", isOpen )
                ]
            ]
            ((List.map viewSelected (List.reverse model.selected))
            ++ [ control
                    Nothing
                    { classes = "cursor-text flex-grow grid grid-cols-[min-content] after:content-[attr(data-value)] after:block after:col-start-1 after:row-start-1 after:pl-3.5 after:text-sm after:invisible"
                    , classList = []
                    , placeholder = "Select"
                    }
                    [ Menus.Combobox.input (Menus.Combobox.WithToken token)
                        Nothing
                        { placeholder = "Select"
                        , classes = "h-[26px] my-1 col-start-1 leading-none row-start-1 w-full min-w-[1px] inline-flex focus:outline-none flex-shrink bg-transparent flex items-center text-sm text-saint-patrick-blue placeholder-silver-sand transition-colors pl-2.5"
                        , classList = []
                        }
                    ]
            ])
        , Menus.Combobox.options token
            { classes = "absolute top-full left-0 min-w-full max-h-48 overflow-y-scroll border border-gray-300 shadow-sm rounded-b-md outline-none focus:outline-none bg-white text-sm text-gray-900 leading-none"
            , classList =
                [ ( "opacity-100 pointer-events-all", isOpen )
                , ( "opacity-0 pointer-events-none", not isOpen )
                ]
            }
            (case isOpen of
                True ->
                    List.filter (\opt -> not (List.member opt model.selected)) model.options
                        |> List.map viewOption

                False ->
                    []
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.Combobox.subscriptions model.menu menuMsgConfig
