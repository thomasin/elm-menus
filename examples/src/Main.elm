module Main exposing (Model, Msg(..), main)

import Browser
import Examples.AutoSelectingCombobox
import Examples.BasicCombobox
import Examples.BasicListbox
import Examples.BasicMenu
import Examples.CreatableCombobox
import Examples.MultiSelectCombobox
import Html
import Html.Attributes as Attr
import Platform



--


type alias Model =
    { basicCombobox : Examples.BasicCombobox.Model
    , autoSelectingCombobox : Examples.AutoSelectingCombobox.Model
    , creatableCombobox : Examples.CreatableCombobox.Model
    , multiSelectCombobox : Examples.MultiSelectCombobox.Model
    , basicMenu : Examples.BasicMenu.Model
    , basicListbox : Examples.BasicListbox.Model
    }


type Msg
    = BasicCombobox Examples.BasicCombobox.Msg
    | AutoSelectingCombobox Examples.AutoSelectingCombobox.Msg
    | CreatableCombobox Examples.CreatableCombobox.Msg
    | MultiSelectCombobox Examples.MultiSelectCombobox.Msg
    | BasicMenu Examples.BasicMenu.Msg
    | BasicListbox Examples.BasicListbox.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( basicComboboxMsg, basicComboboxCmd ) =
            Examples.BasicCombobox.init

        ( creatableComboboxMsg, creatableComboboxCmd ) =
            Examples.CreatableCombobox.init

        ( autoSelectingComboboxMsg, autoSelectingComboboxCmd ) =
            Examples.AutoSelectingCombobox.init

        ( multiSelectComboboxMsg, multiSelectComboboxCmd ) =
            Examples.MultiSelectCombobox.init

        ( basicMenuMsg, basicMenuCmd ) =
            Examples.BasicMenu.init

        ( basicListboxMsg, basicListboxCmd ) =
            Examples.BasicListbox.init
    in
    ( { basicCombobox = basicComboboxMsg
      , autoSelectingCombobox = autoSelectingComboboxMsg
      , creatableCombobox = creatableComboboxMsg
      , multiSelectCombobox = multiSelectComboboxMsg
      , basicMenu = basicMenuMsg
      , basicListbox = basicListboxMsg
      }
    , Cmd.batch
        [ Cmd.map BasicCombobox basicComboboxCmd
        , Cmd.map AutoSelectingCombobox autoSelectingComboboxCmd
        , Cmd.map CreatableCombobox creatableComboboxCmd
        , Cmd.map MultiSelectCombobox multiSelectComboboxCmd
        , Cmd.map BasicMenu basicMenuCmd
        , Cmd.map BasicListbox basicListboxCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BasicCombobox msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.BasicCombobox.update msg_ model.basicCombobox
            in
            ( { model | basicCombobox = model_ }
            , Cmd.map BasicCombobox cmd_
            )

        AutoSelectingCombobox msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.AutoSelectingCombobox.update msg_ model.autoSelectingCombobox
            in
            ( { model | autoSelectingCombobox = model_ }
            , Cmd.map AutoSelectingCombobox cmd_
            )

        CreatableCombobox msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.CreatableCombobox.update msg_ model.creatableCombobox
            in
            ( { model | creatableCombobox = model_ }
            , Cmd.map CreatableCombobox cmd_
            )

        MultiSelectCombobox msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.MultiSelectCombobox.update msg_ model.multiSelectCombobox
            in
            ( { model | multiSelectCombobox = model_ }
            , Cmd.map MultiSelectCombobox cmd_
            )

        BasicMenu msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.BasicMenu.update msg_ model.basicMenu
            in
            ( { model | basicMenu = model_ }
            , Cmd.map BasicMenu cmd_
            )

        BasicListbox msg_ ->
            let
                ( model_, cmd_ ) =
                    Examples.BasicListbox.update msg_ model.basicListbox
            in
            ( { model | basicListbox = model_ }
            , Cmd.map BasicListbox cmd_
            )


view : Model -> Browser.Document Msg
view model =
    let
        label : (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg) -> String -> String -> String -> Html.Html Msg
        label headingLevel str url description =
            Html.div
                []
                [ headingLevel
                    [ Attr.class "text-sm font-medium mb-1" ]
                    (case url of
                        "" ->
                            [ Html.text str
                            ]

                        _ ->
                            [ Html.text str
                            , Html.span
                                [ Attr.class "inline-block mx-1" ]
                                [ Html.text "•" ]
                            , Html.a
                                [ Attr.href url
                                , Attr.target "_blank"
                                , Attr.class "text-xs text-blue-600 hover:text-blue-800 transition-colors underline rounded leading-none ring-0 focus:ring ring-blue-100 ring-offset-2"
                                ]
                                [ Html.text url ]
                            ]
                    )
                , case String.trim description of
                    "" ->
                        Html.text ""

                    trimmed ->
                        Html.span
                            [ Attr.class "mt-0.5 mb-2 block text-xs text-stone-500" ]
                            [ Html.text trimmed
                            ]
                ]
    in
    { title = "🌸"
    , body =
        [ Html.h1
            [ Attr.class "border-b border-stone-50 p-8 font-medium" ]
            [ Html.text "Menus" ]
        , Html.div
            [ Attr.class "p-8" ]
            [ label Html.h2 "Combobox" "https://www.w3.org/WAI/ARIA/apg/patterns/combobox/" ""
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-stone-50 rounded p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Single select" "" "A simple combobox"
                    , Html.map BasicCombobox (Examples.BasicCombobox.view model.basicCombobox)
                    ]
                , Html.div
                    []
                    [ label Html.h3 "Auto selecting + not clearable" "" "Automatically select the closest match to the input"
                    , Html.map AutoSelectingCombobox (Examples.AutoSelectingCombobox.view model.autoSelectingCombobox)
                    ]
                , Html.div
                    []
                    [ label Html.h3 "Creatable + auto selecting" "" "Create new options"
                    , Html.map CreatableCombobox (Examples.CreatableCombobox.view model.creatableCombobox)
                    ]
                , Html.div
                    []
                    [ label Html.h3 "Multi select" "" "Choose more than one option (currently not exposed as a Preset, need to finish the docs!)"
                    , Html.map MultiSelectCombobox (Examples.MultiSelectCombobox.view model.multiSelectCombobox)
                    ]
                ]
            ]
        , Html.div
            [ Attr.class "p-8" ]
            [ label Html.h2 "Listbox" "https://www.w3.org/WAI/ARIA/apg/patterns/listbox/" ""
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-stone-50 rounded p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Single select" "" ""
                    , Html.map BasicListbox (Examples.BasicListbox.view model.basicListbox)
                    ]
                ]
            ]
        , Html.div
            [ Attr.class "p-8 mb-[300px]" ]
            [ label Html.h2 "Menu" "https://www.w3.org/WAI/ARIA/apg/patterns/menu-button/" ""
            , Html.div
                [ Attr.class "grid grid-cols-2 gap-y-8 bg-stone-50 rounded p-8" ]
                [ Html.div
                    []
                    [ label Html.h3 "Menu" "" ""
                    , Html.map BasicMenu (Examples.BasicMenu.view model.basicMenu)
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
