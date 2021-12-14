module Main exposing (..)

import Examples.BasicCombobox
import Examples.MultiCombobox
import Examples.BasicMenu
import Examples.BasicListbox
import Browser
import Html
import Html.Attributes as Attr


--


type alias Model =
    { basicCombobox : Examples.BasicCombobox.Model
    , multiCombobox : Examples.MultiCombobox.Model
    , basicMenu : Examples.BasicMenu.Model
    , basicListbox : Examples.BasicListbox.Model
    }


type Msg
    = BasicCombobox Examples.BasicCombobox.Msg
    | MultiCombobox Examples.MultiCombobox.Msg
    | BasicMenu Examples.BasicMenu.Msg
    | BasicListbox Examples.BasicListbox.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( basicComboboxMsg, basicComboboxCmd ) = Examples.BasicCombobox.init
        ( multiComboboxMsg, multiComboboxCmd ) = Examples.MultiCombobox.init
        ( basicMenuMsg, basicMenuCmd ) = Examples.BasicMenu.init
        ( basicListboxMsg, basicListboxCmd ) = Examples.BasicListbox.init
    in
    ( { basicCombobox = basicComboboxMsg
      , multiCombobox = multiComboboxMsg
      , basicMenu = basicMenuMsg
      , basicListbox = basicListboxMsg
      }
    , Cmd.batch
        [ Cmd.map BasicCombobox basicComboboxCmd
        , Cmd.map MultiCombobox multiComboboxCmd
        , Cmd.map BasicMenu basicMenuCmd
        , Cmd.map BasicListbox basicListboxCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BasicCombobox msg_ ->
            let ( model_, cmd_ ) = Examples.BasicCombobox.update msg_ model.basicCombobox in
            ( { model | basicCombobox = model_ }, Cmd.map BasicCombobox cmd_ )

        MultiCombobox msg_ ->
            let ( model_, cmd_ ) = Examples.MultiCombobox.update msg_ model.multiCombobox in
            ( { model | multiCombobox = model_ }, Cmd.map MultiCombobox cmd_ )

        BasicMenu msg_ ->
            let ( model_, cmd_ ) = Examples.BasicMenu.update msg_ model.basicMenu in
            ( { model | basicMenu = model_ }, Cmd.map BasicMenu cmd_ )

        BasicListbox msg_ ->
            let ( model_, cmd_ ) = Examples.BasicListbox.update msg_ model.basicListbox in
            ( { model | basicListbox = model_ }, Cmd.map BasicListbox cmd_ )


view : Model -> Browser.Document Msg
view model =
    let
        label headingLevel str url =
            headingLevel
                [ Attr.class "text-sm font-medium mb-1" ]
                ( case url of
                    "" ->
                        [ Html.text str ]

                    _ ->
                        [ Html.text str
                        , Html.span
                            [ Attr.class "inline-block mx-1" ]
                            [ Html.text "•" ]
                        , Html.a
                            [ Attr.href url
                            , Attr.target "_blank"
                            , Attr.class "text-xs text-blue-600"
                            ]
                            [ Html.text url ]
                        ]
                )
    in
    { title = "🌸"
    , body =
        [ Html.h1
            [ Attr.class "border-b border-gray-50 p-8 font-medium" ]
            [ Html.text "Menus" ]
        , Html.div
            [ Attr.class "p-8" ]
            [ label Html.h2 "Combobox" "https://www.w3.org/TR/wai-aria-practices/#combobox"
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-gray-50 p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Single select" ""
                    , Html.map BasicCombobox (Examples.BasicCombobox.view model.basicCombobox)
                    ]
                , Html.div
                    []
                    [ label Html.h3 "Multi select" ""
                    , Html.map MultiCombobox (Examples.MultiCombobox.view model.multiCombobox)
                    ]
                ]
            ]
        , Html.div
            [ Attr.class "p-8" ]
            [ label Html.h2 "Listbox" "https://www.w3.org/TR/wai-aria-practices/#Listbox"
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-gray-50 p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Single select" ""
                    , Html.map BasicListbox (Examples.BasicListbox.view model.basicListbox)
                    ]
                ]
            ]
        , Html.div
            [ Attr.class "p-8" ]
            [ label Html.h2 "Menu" "https://www.w3.org/TR/wai-aria-practices/#menu"
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-gray-50 p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Menu" ""
                    , Html.map BasicMenu (Examples.BasicMenu.view model.basicMenu)
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map BasicCombobox (Examples.BasicCombobox.subscriptions model.basicCombobox)
        , Sub.map MultiCombobox (Examples.MultiCombobox.subscriptions model.multiCombobox)
        , Sub.map BasicMenu (Examples.BasicMenu.subscriptions model.basicMenu)
        , Sub.map BasicListbox (Examples.BasicListbox.subscriptions model.basicListbox)
        ]


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
