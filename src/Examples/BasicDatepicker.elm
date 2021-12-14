module Examples.BasicDatepicker exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Date
import Time
import Task

import Menus.Datepicker
import Menus.ComboboxWrap


--


type alias Model =
    { today : Date.Date
    , menu : Menus.Datepicker.State
    , selected : Maybe Date.Date
    }


type Msg
    = TodayFound Date.Date
    | YearChanged Int
    | MonthChanged Int
    | MenuOpened
    | MenuClosed
    | MenuFocussed (Menus.ComboboxWrap.Focussed Date.Date)
    | MenuSelected (Menus.ComboboxWrap.Selected Date.Date)
    | MenuInputted Menus.ComboboxWrap.Inputted
    | NoOp


menuConfig : Date.Date -> Menus.ComboboxWrap.Config () Date.Date Date.Date (Maybe Date.Date)
menuConfig today =
    Menus.Datepicker.combobox today
        { id = "basic-datepicker"
        , optionToLabel = Date.toIsoString
        , optionToValue = identity
        , valueToString = Date.format "Dy"
        }


menuMsgConfig : Menus.Datepicker.MsgConfig Msg
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
        { today = Date.fromCalendarDate 2000 Time.Jan 1
        , menu = Menus.ComboboxWrap.init
        , selected = Nothing
        }
    , Task.perform TodayFound Date.today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodayFound today ->
            ( { model | today = today }, Cmd.none )

        MonthChanged diff ->
            ( { model | menu = Menus.ComboboxWrap.setMatch model.menu (Date.add Date.Months diff << Maybe.withDefault model.today) }, Cmd.none )

        YearChanged diff ->
            ( { model | menu = Menus.ComboboxWrap.setMatch model.menu (Date.add Date.Years diff << Maybe.withDefault model.today) }, Cmd.none )

        MenuOpened ->
            let
                ( model_, cmd_ ) =
                    Menus.ComboboxWrap.opened
                        { state = model.menu
                        , config = menuConfig model.today
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
                    Menus.ComboboxWrap.closed
                        { config = menuConfig model.today
                        , msgConfig = menuMsgConfig
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuFocussed focussed ->
            let
                ( model_, cmd_ ) =
                    Menus.ComboboxWrap.focussed
                        { msg = focussed
                        , state = model.menu
                        , config = menuConfig model.today
                        , msgConfig = menuMsgConfig
                        , options = ()
                        }
            in
            ( { model | menu = model_ }
            , cmd_
            )

        MenuSelected selected ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.ComboboxWrap.selected
                        { msg = selected
                        , state = model.menu
                        , config = menuConfig model.today
                        , msgConfig = menuMsgConfig
                        , options = ()
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        MenuInputted inputted ->
            let
                ( selected_, model_, cmd_ ) =
                    Menus.ComboboxWrap.inputted
                        { msg = inputted
                        , state = model.menu
                        , config = menuConfig model.today
                        , msgConfig = menuMsgConfig
                        , options = ()
                        , selected = model.selected
                        }
            in
            ( { model | menu = model_, selected = selected_ }
            , cmd_
            )

        NoOp ->
            ( model, Cmd.none )


monthToString month =
    case month of
        Time.Jan -> "jan"
        Time.Feb -> "febr"
        Time.Mar -> "mar"
        Time.Apr -> "april"
        Time.May -> "may"
        Time.Jun -> "juni"
        Time.Jul -> "juli"
        Time.Aug -> "august"
        Time.Sep -> "september"
        Time.Oct -> "oct"
        Time.Nov -> "november"
        Time.Dec -> "december"


view model =
    let
        centered = Menus.Datepicker.centered (Menus.ComboboxWrap.lastMatch model.menu) model.today

        isOpen =
            Menus.ComboboxWrap.isOpen model.menu

        headers =
            [ Html.div
                [ Attr.class "col-start-1 col-span-7 flex items-center justify-between" ]
                [ Html.button [ Attr.class "focus:outline-none focus-visible:outline-black leading-none rounded", Events.onClick (YearChanged -1) ] [ Html.text "⬅️" ]
                , Html.text (String.fromInt (Date.year centered))
                , Html.button [ Attr.class "focus:outline-none focus-visible:outline-black leading-none rounded", Events.onClick (YearChanged 1) ] [ Html.text "➡️" ]
                ]
            , Html.div
                [ Attr.class "col-start-1 col-span-7 flex items-center justify-between" ]
                [ Html.button [ Attr.class "focus:outline-none focus-visible:outline-black leading-none rounded", Events.onClick (MonthChanged -1) ] [ Html.text "⬅️" ]
                , Html.text (monthToString (Date.month centered))
                , Html.button [ Attr.class "focus:outline-none focus-visible:outline-black leading-none rounded", Events.onClick (MonthChanged 1) ] [ Html.text "➡️" ]
                ]
            , Html.div [] [ Html.text "sun" ]
            , Html.div [] [ Html.text "mon" ]
            , Html.div [] [ Html.text "tue" ]
            , Html.div [] [ Html.text "wed" ]
            , Html.div [] [ Html.text "thu" ]
            , Html.div [] [ Html.text "fri" ]
            , Html.div [] [ Html.text "sat" ]
            ]

        viewOption date =
            Menus.ComboboxWrap.option
                (menuConfig model.today)
                menuMsgConfig
                date
                (Just date == model.selected)
                { classes = "justify-self-stretch select-none leading-none p-2 text-sm flex justify-center rounded-full focus:outline-none focus-visible:outline-black"
                , classList =
                    [ ( "bg-blue-300", Menus.ComboboxWrap.currentlyFocussed model.menu == Just date )
                    , ( "text-black font-medium", Just date == model.selected )
                    , ( "bg-blue-50 text-blue-900 focus:bg-blue-300", Date.month date == Date.month centered )
                    , ( "bg-gray-100 text-gray-900 focus:bg-gray-300", Date.month date /= Date.month centered )
                    , ( "bg-orange-200", date == model.today )
                    ]
                }
                [ Html.div
                    []
                    [ Html.text (Date.format "d" date)
                    ]
                ]

    in
    Menus.ComboboxWrap.container menuMsgConfig
        { classes = "relative w-72 flex flex-wrap justify-between" }
        [ Html.div
            [ Attr.class "relative w-full h-11 flex items-center border border-gray-300 bg-white rounded shadow-sm"
            , Attr.classList
                [ ( "rounded-b-none border-b-0", isOpen )
                ]
            ]
            [ Menus.ComboboxWrap.input model.menu
                (menuConfig model.today)
                menuMsgConfig
                (Maybe.map (menuConfig model.today).optionToLabel model.selected)
                { placeholder = "Select"
                , classes = "w-full h-full focus:outline-none pl-3.5 bg-transparent flex items-center text-sm text-gray-900 transition-colors rounded focus-visible:outline-black"
                , classList = []
                }
            ]
        , if isOpen then
            Menus.ComboboxWrap.options model.menu
                (menuConfig model.today)
                menuMsgConfig
                model.selected
                { classes = "absolute z-50 top-full left-0 w-full grid grid-cols-7 gap-x-1 gap-y-1 items-center justify-center bg-white border border-gray-300 p-2 rounded-b-lg focus:outline-none focus-visible:outline-black"
                , classList = []
                }
                (headers ++ List.map viewOption (Menus.Datepicker.visibleOptions centered))

         else
            Html.text ""
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Menus.ComboboxWrap.subscriptions model.menu menuMsgConfig
