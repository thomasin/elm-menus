module Preset.Menu exposing
    ( Model, Msg, Config, init, update, config
    , Token, isOpen, focussedOption, view, button, options, link
    )

{-| Menus!

@docs Model, Msg, Config, init, update, config


# Views

The view is highly customisable. You decide how to structure the menu & how it is styled.
There a few caveats here to note:

  - Displaying the menu always open, without [#button](#button) to manage state, is unsupported
    It is possible with a rework, so depends on demand. If this is something you would really want, please open an issue.
  - All of [#button](#button), [#options](#options) and [#link](#link) must be present
  - [#options](#options) is a `ul` node, it must take a list of only [#link](#link) and other `li` nodes to be valid HTML

```
Preset.Menu.view model
    menuConfig
    (\token ->
        Html.div
            [ Attr.class "..."
            ]
            [ Preset.Menu.button token
                [ Attr.class "..."
                ]
                [ Html.text "Examples"
                ]
            , Html.button
                [ Attr.class "..."
                , Key.tabbable False
                ]
                [ if Preset.Menu.isOpen token then
                    Examples.Svg.chevronUp

                  else
                    Examples.Svg.chevronDown
                ]
            , Preset.Menu.options token
                [ Attr.class "..."
                , Attr.classList
                    [ ( "...", Preset.Menu.isOpen token )
                    , ( "...", not (Preset.Menu.isOpen token) )
                    ]
                ]
                (List.indexedMap
                    (\idx option ->
                        Preset.Menu.link token
                            { idx = idx }
                            []
                            (\link ->
                                link
                                    [ Attr.href "/"
                                    , Attr.class "..."
                                    ]
                                    [ Html.text option.label ]
                            )
                    )
                    Examples.MenuItem.list
                )
            ]
    )
```

@docs Token, isOpen, focussedOption, view, button, options, link

-}

import Html
import Menus.Menu
import Preset.Menu.Internal



--


{-| Holds information about whether the menu is open or closed.
Should be kept in your Model.
-}
type Model
    = Model Menus.Menu.State


{-| Create with [#config](#config).
-}
type Config option
    = Config (Menus.Menu.Config (List option))


{-| Passed into the [#update](#update) function.
-}
type alias Msg =
    Preset.Menu.Internal.Msg


{-| Pass in an id to uniquely identity this dropdown.
It _must_ be unique to the page or weird things will happen.
-}
config : String -> Config option
config id =
    Config
        { id = id
        , optionsLength = List.length
        }


{-| Initialise a closed menu
-}
init : Model
init =
    Model Menus.Menu.init


{-| Handles updating menu and focus state.
Call this in your update function.
-}
update : Msg -> { model : Model, config : Config option, options : List option } -> ( Model, Cmd Msg )
update msg args =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config

        (Preset.Menu.Internal.MsgConfig msgConfig) =
            Preset.Menu.Internal.menuMsgConfig
    in
    case msg of
        Preset.Menu.Internal.MenuOpened openDirection ->
            Tuple.mapFirst Model
                (Menus.Menu.opened openDirection
                    { state = model
                    , config = menuConfig
                    , msgConfig = msgConfig
                    , options = args.options
                    }
                )

        Preset.Menu.Internal.MenuClosed ->
            Tuple.mapFirst Model
                (Menus.Menu.closed
                    { config = menuConfig
                    , msgConfig = msgConfig
                    }
                )

        Preset.Menu.Internal.MenuFocussed focussed_ ->
            Tuple.mapFirst Model
                (Menus.Menu.focussed
                    { msg = focussed_
                    , state = model
                    , config = menuConfig
                    , msgConfig = msgConfig
                    , options = args.options
                    }
                )

        Preset.Menu.Internal.NoOp ->
            ( Model model
            , Cmd.none
            )


{-| Passed into your view function
-}
type Token option
    = Token (Menus.Menu.Token (List option) Msg)


{-| Returns whether the menu state is open or closed
You can use this in your view to only show the options list when the menu state is open, or switch chevron direction

    Html.button
        [ Attr.class "..."
        , Key.tabbable False
        ]
        [ if Preset.Menu.isOpen token then
            Examples.Svg.chevronUp

          else
            Examples.Svg.chevronDown
        ]

-}
isOpen : Token option -> Bool
isOpen (Token token) =
    token.isOpen


{-| Returns the currently focussed option. This provides consistency with other dropdowns
like [Preset.Combobox](Preset-Combobox) or [Preset.Listbox](Preset-Listbox), but menu items
are different from the other dropdowns in that they are focussed directly, so for styling you just need
to use a CSS focus selector.

    link
        [ Attr.href "/"
        , Attr.class "bg-transparent focus:bg-purple-100"
        ]
        [ Html.text option.label ]

-}
focussedOption : Token option -> Maybe Int
focussedOption (Token token) =
    token.focussed


{-| This takes the menu model & config and gives you a token
you can pass into the functions that make up a complete menu.
Use [#button](#button), [#options](#options) and [#link](#link) to form a complete menu

    Preset.Menu.view model menuConfig
        (\token ->
            ... view code here
        )

-}
view : { model : Model, config : Config option } -> (Token option -> Html.Html Msg) -> Html.Html Msg
view args func =
    let
        (Model model) =
            args.model

        (Config menuConfig) =
            args.config

        (Preset.Menu.Internal.MsgConfig msgConfig) =
            Preset.Menu.Internal.menuMsgConfig

        token : Token option
        token =
            Token
                (Menus.Menu.menuToken
                    { state = model
                    , config = menuConfig
                    , msgConfig = msgConfig
                    }
                )
    in
    func token


{-| The button used to open and close the menu

    Preset.Menu.button token
        [ Attr.class "..."
        ]
        [ Html.text "Examples"
        ]

-}
button : Token option -> (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg)
button (Token token) =
    Menus.Menu.button token


{-| A wrapper for the list of options in the menu.
This should be a direct parent of your list of option nodes.

    Preset.Menu.options token
        [ Attr.class "..."
        , Attr.classList
            [ ( "...", Preset.Menu.isOpen token )
            , ( "...", not (Preset.Menu.isOpen token) )
            ]
        ]
        (List.indexedMap
            (\idx option ->
                Preset.Menu.link token
                    { idx = idx }
                    []
                    (\link ->
                        link
                            [ Attr.href "/"
                            , Attr.class "bg-transparent focus:bg-purple-100"
                            ]
                            [ Html.text option.label ]
                    )
            )
            Examples.MenuItem.list
        )

-}
options : Token option -> (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg)
options (Token token) =
    Menus.Menu.options token


{-| Currently menus only support link children
Takes the token returned from [#view](#view), the current option index, a list of HTML attributes, and a function.
The function argument is in effect an Html.a node and can be used like a normal HTML node, although cannot be nested.

    Preset.Menu.link token
        { idx = idx }
        []
        (\link ->
            link
                [ Attr.href "/"
                , Attr.class "bg-transparent focus:bg-purple-100"
                ]
                [ Html.text option.label ]
        )

-}
link : Token options -> { idx : Int } -> List (Html.Attribute Msg) -> ((List (Html.Attribute Msg) -> List (Html.Html Never) -> Menus.Menu.Link Msg) -> Menus.Menu.Link Msg) -> Html.Html Msg
link (Token token) =
    Menus.Menu.link token
