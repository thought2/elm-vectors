module Vec2Test exposing (..)

import Fuzz exposing (..)
import Test exposing (..)


map : Test
map =
    describe
        "map"
        [ fuzz float <|
            \fl -> map () Vec2
        ]
