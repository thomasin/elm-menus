module Examples.BasicCombobox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
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
    ( { menu = Menus.Combobox.init
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


view : Model -> Html.Html Msg
view model =
    let
        token : Menus.Combobox.Token (List Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Examples.MenuItem.MenuItem (Maybe Examples.MenuItem.MenuItem) Msg
        token =
            Menus.Combobox.menuToken
                { state = model.menu
                , config = menuConfig
                , msgConfig = menuMsgConfig
                , selected = model.selected
                }
    in
    Html.div
        [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between relative h-11 items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
        ]
        [ Menus.Combobox.input token
            { placeholder = "Select" }
            [ Attr.class "w-full h-full rounded bg-transparent flex items-center px-3.5 text-sm placeholder-stone-400" ]
        , Html.button
            [ Attr.class "absolute right-2 w-5 text-purple-900"

            --, Html.Events.onClick (if Menus.Combobox.isOpen model.menu then menuMsgConfig.onClosed else menuMsgConfig.onOpened)
            ]
            [ if Menus.Combobox.isOpen model.menu then
                Examples.Svg.upArrow

              else
                Examples.Svg.downArrow
            ]
        , Menus.Combobox.options token
            [ Attr.class "absolute top-full left-0 min-w-full max-h-48 overflow-y-scroll mt-2 shadow-md rounded border border-stone-300 p-1 pt-0 bg-white text-sm text-stone-900 transition"
            , Attr.classList
                [ ( "z-50 opacity-100 pointer-events-all", Menus.Combobox.isOpen model.menu )
                , ( "z-0 opacity-0 pointer-events-none", not (Menus.Combobox.isOpen model.menu) )
                ]
            ]
            (if Menus.Combobox.isOpen model.menu then
                List.map
                    (\option ->
                        Menus.Combobox.option token
                            { value = option, isSelected = Just option == model.selected }
                            [ Attr.class "pt-1" ]
                            [ Html.div
                                [ Attr.class "flex cursor-default rounded px-3.5 py-3 transition-colors"
                                , Attr.classList
                                    [ ( "bg-purple-100", Just option == token.focussed )
                                    , ( "bg-transparent", not (Just option == token.focussed) )
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
                    model.options

             else
                []
            )
        ]
