module Vec2Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)
import Types exposing (..)
import Vec2


map : Test
map =
    describe
        "map"
        [ test "maps over all fields" <|
            \_ ->
                Vec2.map ((+) 1) (Vec2 1 2)
                    |> Expect.equal (Vec2 2 3)
        ]


pure : Test
pure =
    describe
        "pure"
        [ test "initializes all fields" <|
            \_ ->
                Vec2.pure 1
                    |> Expect.equal (Vec2 1 1)
        ]
