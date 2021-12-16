module Examples.BasicListbox exposing (Model, Msg, init, update, view)

import Examples.MenuItem
import Examples.Svg
import Html
import Html.Attributes as Attr
import Menus.Listbox
import ZipList



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
    | MenuInputted Menus.Listbox.Inputted
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
    , onInputted = MenuInputted
    , onNoOp = NoOp
    }


init : ( Model, Cmd Msg )
init =
    ( { menu = Menus.Listbox.closed
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

        MenuInputted inputted ->
            let
                ( model_, cmd_ ) =
                    Menus.Listbox.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = menuConfig
                        , msgConfig = menuMsgConfig
                        , options = model.options
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
        token : Menus.Listbox.Token (ZipList.ZipList Examples.MenuItem.MenuItem) Examples.MenuItem.MenuItem Int (ZipList.ZipList Examples.MenuItem.MenuItem) Msg
        token =
            Menus.Listbox.menuToken
                { state = model.menu
                , config = menuConfig
                , msgConfig = menuMsgConfig
                , selected = model.options
                }
    in
    Html.div
        [ Attr.class "relative w-72 h-11 flex flex-wrap justify-between relative h-11 flex items-center border border-stone-300 bg-white rounded shadow-sm ring-0 focus-within:ring ring-purple-100 ring-offset-stone-50 ring-offset-1"
        ]
        [ Menus.Listbox.button token
            [ Attr.class "w-full h-full rounded bg-transparent flex items-center px-3.5 text-sm" ]
            [ Html.text (menuConfig.optionToLabel (ZipList.current model.options))
            ]
        , Html.button
            [ Attr.class "absolute cursor-pointer right-2 w-5 pointer-events-none text-purple-900"
            ]
            [ if Menus.Listbox.isOpen model.menu then
                Examples.Svg.chevronUp

              else
                Examples.Svg.chevronDown
            ]
        , Menus.Listbox.options token
            [ Attr.class "absolute top-full left-0 min-w-full mt-2 shadow-md rounded border border-stone-300 p-1 space-y-1 bg-white text-sm text-stone-900 transition"
            , Attr.classList
                [ ( "z-50 opacity-100 pointer-events-all", Menus.Listbox.isOpen model.menu )
                , ( "z-20 opacity-0 pointer-events-none", not (Menus.Listbox.isOpen model.menu) )
                ]
            ]
            (ZipList.toList
                (ZipList.indexedSelectedMap
                    (\idx isSelected option ->
                        Menus.Listbox.option token
                            { value = idx, isSelected = isSelected }
                            [ Attr.class "cursor-default rounded flex pl-2 pr-3 py-2 transition-colors"
                            , Attr.classList
                                [ ( "bg-purple-100", Just idx == token.focussed )
                                , ( "bg-transparent", not (Just idx == token.focussed) )
                                ]
                            ]
                            [ Html.div
                                [ Attr.class "w-5 flex justify-center mr-2 text-purple-900"
                                ]
                                [ if isSelected then
                                    Examples.Svg.check

                                  else
                                    Html.text ""
                                ]
                            , Html.text (menuConfig.optionToLabel option)
                            ]
                    )
                    model.options
                )
            )
        ]
