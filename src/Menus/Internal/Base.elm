module Menus.Internal.Base exposing (Direction(..))

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import List.Extra as List
import Task
import Menus.Internal.KeyEvent


type Direction
    = Up | Right | Down | Left