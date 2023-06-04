module Examples.MenuItem exposing (MenuItem, list, nonEmpty)


type alias MenuItem =
    { id : Int
    , emoji : String
    , label : String
    }


nonEmpty : ( MenuItem, List MenuItem )
nonEmpty =
    ( { id = 1, emoji = "🥧", label = "Pie" }
    , [ { id = 2, emoji = "🥞", label = "Pancakes" }
      , { id = 3, emoji = "🌶", label = "Chili" }
      , { id = 4, emoji = "🥯", label = "Bagel" }
      , { id = 5, emoji = "🥕", label = "Carrot" }
      , { id = 6, emoji = "🍎", label = "Apple" }
      ]
    )


list : List MenuItem
list =
    Tuple.first nonEmpty :: Tuple.second nonEmpty
