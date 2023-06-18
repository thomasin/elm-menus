module Menus.Combobox.Internal exposing (Config, Emptiable(..), EmptiableConfig, focusChange, focusOnControl, id, ids, optionToValue, selectChange, selectValue, selectionToLabel, selectionToOption, valueToString)

import Browser.Dom
import Menus.Active
import Menus.Focus
import Menus.Select
import Menus.Select.Internal
import Task


ids : { control : String -> String, options : String -> String, option : String -> String -> String }
ids =
    { control = \id_ -> id_ ++ "-control"
    , options = \id_ -> id_ ++ "-options"
    , option = \id_ str -> id_ ++ "-option-" ++ str
    }


focusOnControl : String -> msg -> Cmd msg
focusOnControl id_ onNoOp =
    Browser.Dom.focus (ids.control id_)
        |> Task.attempt (always onNoOp)


type alias Config options option value selected =
    Emptiable (EmptiableConfig options option value selected)


type Emptiable emptiable
    = Emptiable emptiable


type alias EmptiableConfig options option value selected =
    { id : String
    , empty : options
    , optionToValue : option -> value
    , valueToString : value -> String
    , selectionToLabel : selected -> String
    , selectionToOption : selected -> options -> Maybe option
    , selectChange : Menus.Select.Internal.SelectAction -> selected -> options -> Menus.Select.Change selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
    , visibleOptions : String -> selected -> options -> Maybe ( Menus.Active.Active option, options )
    }



-- type alias NonEmptiableConfig options option value selected =
--     { id : String
--     , optionToValue : option -> value
--     , valueToString : value -> String
--     , selectionToLabel : selected -> String
--     , selectionToOption : selected -> options -> Maybe option
--     , selectChange : Menus.Select.Internal.SelectAction -> selected -> options -> Menus.Select.Change selected
--     , selectValue : value -> selected -> options -> selected
--     , focusChange : Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
--     , visibleOptions : String -> selected -> options -> ( Menus.Active.Active option, options )
--     }


id : Config options option value selected -> String
id config =
    case config of
        Emptiable emptiable ->
            emptiable.id


optionToValue : Config options option value selected -> option -> value
optionToValue config option =
    case config of
        Emptiable emptiable ->
            emptiable.optionToValue option


valueToString : Config options option value selected -> value -> String
valueToString config value =
    case config of
        Emptiable emptiable ->
            emptiable.valueToString value


selectionToLabel : Config options option value selected -> selected -> String
selectionToLabel config selection =
    case config of
        Emptiable emptiable ->
            emptiable.selectionToLabel selection


selectionToOption : Config options option value selected -> selected -> options -> Maybe option
selectionToOption config selection options =
    case config of
        Emptiable emptiable ->
            emptiable.selectionToOption selection options


selectChange : Config options option value selected -> Menus.Select.Internal.SelectAction -> selected -> options -> Menus.Select.Change selected
selectChange config action selection options =
    case config of
        Emptiable emptiable ->
            emptiable.selectChange action selection options


selectValue : Config options option value selected -> value -> selected -> options -> selected
selectValue config value selected options =
    case config of
        Emptiable emptiable ->
            emptiable.selectValue value selected options


focusChange : Config options option value selected -> Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
focusChange config action focus options =
    case config of
        Emptiable emptiable ->
            emptiable.focusChange action focus options
