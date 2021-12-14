module Examples.Common exposing (container)


container menuMsgConfig =
    Menus.ComboboxWrap.container menuMsgConfig
        { classes = "relative w-72 flex justify-between" }
