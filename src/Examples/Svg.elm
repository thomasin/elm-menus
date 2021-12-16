module Examples.Svg exposing (check, downArrow, upArrow)

import Html
import Svg
import Svg.Attributes


downArrow : Html.Html msg
downArrow =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -4.5 24 24"
        , Svg.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M8 11.243l3.95-3.95a1 1 0 1 1 1.414 1.414l-5.657 5.657a.997.997 0 0 1-1.414 0L.636 8.707A1 1 0 1 1 2.05 7.293L6 11.243V1.657a1 1 0 1 1 2 0v9.586z"
            ]
            []
        ]


upArrow : Html.Html msg
upArrow =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -4.5 24 24"
        , Svg.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M6 4.071l-3.95 3.95A1 1 0 0 1 .636 6.607L6.293.95a.997.997 0 0 1 1.414 0l5.657 5.657A1 1 0 0 1 11.95 8.02L8 4.07v9.586a1 1 0 1 1-2 0V4.07z"
            ]
            []
        ]


check : Html.Html msg
check =
    Svg.svg
        [ Svg.Attributes.viewBox "-5 -7 24 24"
        , Svg.Attributes.fill "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M5.486 9.73a.997.997 0 0 1-.707-.292L.537 5.195A1 1 0 1 1 1.95 3.78l3.535 3.535L11.85.952a1 1 0 0 1 1.415 1.414L6.193 9.438a.997.997 0 0 1-.707.292z"
            ]
            []
        ]
