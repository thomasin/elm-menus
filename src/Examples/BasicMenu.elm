module Examples.BasicMenu exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Html.Events
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


view : Model -> Html.Html Msg
view model =
    let
        token : Menus.Menu.Token (List Examples.MenuItem.MenuItem) Msg
        token =
            Menus.Menu.menuToken
                { state = model.menu
                , config = menuConfig
                , msgConfig = menuMsgConfig
                }
    in
    Html.div
        [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between relative h-11 flex items-center border border-stone-300 bg-transparent rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
        ]
        [ Menus.Menu.button token
            [ Attr.class "cursor-default h-full w-full flex items-center leading-none justify-between px-3.5 text-sm text-stone-900"
            ]
            [ Html.text "Examples"
            ]
        , Html.button
            [ Attr.class "absolute cursor-pointer right-2 w-5 text-purple-900 pointer-events-none"
            ]
            [ if token.isOpen then
                Examples.Svg.chevronUp

              else
                Examples.Svg.chevronDown
            ]
        , Menus.Menu.options token
            [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md rounded border border-stone-300 p-1 space-y-1 bg-white text-sm text-stone-900 transition"
            , Attr.classList
                [ ( "z-50 opacity-100 pointer-events-all", token.isOpen )
                , ( "z-0 opacity-0 pointer-events-none", not token.isOpen )
                ]
            ]
            (List.indexedMap
                (\idx option ->
                    Menus.Menu.option token
                        { idx = idx }
                        [ Attr.class "cursor-pointer rounded px-3 py-2 transition-colors"
                        , Attr.classList
                            [ ( "bg-purple-100", Just idx == token.focussed )
                            , ( "bg-transparent", not (Just idx == token.focussed) )
                            ]
                        , Html.Events.onClick menuMsgConfig.onClosed
                        ]
                        [ Html.text option.label ]
                )
                Examples.MenuItem.list
            )
        ]
