module Examples.Svg exposing (check, chevronDown, chevronUp)

import Html
import Svg
import Svg.Attributes


chevronDown : Html.Html msg
chevronDown =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -8 24 24"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class "w-full h-full"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M7.071 5.314l4.95-4.95a1 1 0 1 1 1.414 1.414L7.778 7.435a1 1 0 0 1-1.414 0L.707 1.778A1 1 0 1 1 2.121.364l4.95 4.95z"
            ]
            []
        ]


chevronUp : Html.Html msg
chevronUp =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -7.5 24 24"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class "w-full h-full"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M7.071 2.828l-4.95 4.95A1 1 0 0 1 .707 6.364L6.364.707a1 1 0 0 1 1.414 0l5.657 5.657a1 1 0 0 1-1.414 1.414l-4.95-4.95z"
            ]
            []
        ]


check : Html.Html msg
check =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -7 24 24"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class "w-full h-full"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M5.486 9.73a.997.997 0 0 1-.707-.292L.537 5.195A1 1 0 1 1 1.95 3.78l3.535 3.535L11.85.952a1 1 0 0 1 1.415 1.414L6.193 9.438a.997.997 0 0 1-.707.292z"
            ]
            []
        ]
