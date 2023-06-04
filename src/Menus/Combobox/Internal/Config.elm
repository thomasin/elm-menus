module Menus.Combobox.Internal.Config exposing (..)

 
import Menus.Select
import Menus.Focus
import Menus.Active


type alias Config options option value selected =
    Emptiable (EmptiableConfig options option value selected) (NonEmptiableConfig options option value selected)


type Emptiable emptiable nonEmptiable
    = Emptiable emptiable
    | NonEmptiable nonEmptiable


type alias EmptiableConfig options option value selected =
    { id : String
    , empty : options
    , optionToLabel : option -> String
    , optionToValue : option -> value
    , valueToString : value -> String
    , selectionToLabel : selected -> String
    , selectionToOption : selected -> Maybe option
    , selectChange : Menus.Select.SelectAction -> selected -> options -> Menus.Select.Change selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
    , visibleOptions : String -> selected -> options -> Maybe ( Menus.Active.Active option, options )
    }


type alias NonEmptiableConfig options option value selected =
    { id : String
    , optionToLabel : option -> String
    , optionToValue : option -> value
    , valueToString : value -> String
    , selectionToLabel : selected -> String
    , selectionToOption : selected -> Maybe option
    , selectChange : Menus.Select.SelectAction -> selected -> options -> Menus.Select.Change selected
    , selectValue : value -> selected -> options -> selected
    , focusChange : Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
    , visibleOptions : String -> selected -> options -> ( Menus.Active.Active option, options )
    }


id : Config options option value selected -> String
id config =
    case config of
        Emptiable emptiable ->
            emptiable.id

        NonEmptiable nonEmptiable ->
            nonEmptiable.id


optionToLabel : Config options option value selected -> option -> String
optionToLabel config option =
    case config of
        Emptiable emptiable ->
            emptiable.optionToLabel option

        NonEmptiable nonEmptiable ->
            nonEmptiable.optionToLabel option


optionToValue : Config options option value selected -> option -> value
optionToValue config option =
    case config of
        Emptiable emptiable ->
            emptiable.optionToValue option

        NonEmptiable nonEmptiable ->
            nonEmptiable.optionToValue option


valueToString : Config options option value selected -> value -> String
valueToString config value =
    case config of
        Emptiable emptiable ->
            emptiable.valueToString value

        NonEmptiable nonEmptiable ->
            nonEmptiable.valueToString value


selectionToLabel : Config options option value selected -> selected -> String
selectionToLabel config selection =
    case config of
        Emptiable emptiable ->
            emptiable.selectionToLabel selection

        NonEmptiable nonEmptiable ->
            nonEmptiable.selectionToLabel selection


selectionToOption : Config options option value selected -> selected -> Maybe option
selectionToOption config selection =
    case config of
        Emptiable emptiable ->
            emptiable.selectionToOption selection

        NonEmptiable nonEmptiable ->
            nonEmptiable.selectionToOption selection


selectChange : Config options option value selected -> Menus.Select.SelectAction -> selected -> options -> Menus.Select.Change selected
selectChange config action selection options =
    case config of
        Emptiable emptiable ->
            emptiable.selectChange action selection options

        NonEmptiable nonEmptiable ->
            nonEmptiable.selectChange action selection options


selectValue : Config options option value selected -> value -> selected -> options -> selected
selectValue config value selected options =
    case config of
        Emptiable emptiable ->
            emptiable.selectValue value selected options

        NonEmptiable nonEmptiable ->
            nonEmptiable.selectValue value selected options


focusChange : Config options option value selected -> Menus.Focus.FocusAction -> Menus.Focus.Focus value -> options -> Menus.Focus.Focus option
focusChange config action focus options =
    case config of
        Emptiable emptiable ->
            emptiable.focusChange action focus options

        NonEmptiable nonEmptiable ->
            nonEmptiable.focusChange action focus options
