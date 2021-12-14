module Menus.Datepicker exposing (Config, MsgConfig, State, combobox, visibleOptions, centered)

import Time
import Date
import Html
import Html.Attributes as Attr
import Debug

import Menus.ComboboxWrap


type alias Config = Menus.ComboboxWrap.Config () Date.Date Date.Date (Maybe Date.Date)


centered : Maybe Date.Date -> Date.Date -> Date.Date
centered lastMatch today =
    Maybe.withDefault today lastMatch


visibleOptions : Date.Date -> List Date.Date
visibleOptions date =
    let
        startOfWeek = Date.Sunday
        startOfMonth = Date.floor Date.Month date
        startOfCalendarMonth = Date.floor startOfWeek startOfMonth
        endOfMonth = Date.add Date.Months 1 startOfMonth
        endOfCalendarMonth = Date.ceiling startOfWeek endOfMonth
    in
        Date.range Date.Day 1 startOfCalendarMonth endOfCalendarMonth


combobox : Date.Date -> { id : String, optionToLabel : Date.Date -> String, optionToValue : Date.Date -> Date.Date, valueToString : Date.Date -> String } -> Config
combobox today config =
    { id = config.id
    , optionToLabel = config.optionToLabel
    , optionToValue = config.optionToValue
    , valueToString = config.valueToString
    , selectionToOption = identity
    , selectChange = \direction value _ ->
        case direction of
            Menus.ComboboxWrap.Left ->
                Maybe.map (Date.add Date.Days -1) value

            Menus.ComboboxWrap.Right ->
                Maybe.map (Date.add Date.Days 1) value

            Menus.ComboboxWrap.Up ->
                Nothing

            Menus.ComboboxWrap.Down ->
                Nothing
    , selectValue = \value _ _ ->
        Just value
    , focusChange = \direction value _ ->
        case direction of
            Menus.ComboboxWrap.Up ->
                Maybe.map (Date.add Date.Weeks -1) value

            Menus.ComboboxWrap.Down ->
                Maybe.map (Date.add Date.Weeks 1) value

            Menus.ComboboxWrap.Left ->
                Maybe.map (Date.add Date.Days -1) value

            Menus.ComboboxWrap.Right ->
                Maybe.map (Date.add Date.Days 1) value
    , focusMatch = \str _ ->
        case Date.fromIsoString str of
            Ok date ->
                Just date

            Err _ ->
                Nothing
    }


type alias MsgConfig msg =
    { onOpened : msg
    , onClosed : msg
    , onFocussed : Menus.ComboboxWrap.Focussed Date.Date -> msg
    , onSelected : Menus.ComboboxWrap.Selected Date.Date -> msg
    , onInput : Menus.ComboboxWrap.Inputted -> msg
    , onNoOp : msg
    }


type alias State = Menus.ComboboxWrap.State Date.Date


