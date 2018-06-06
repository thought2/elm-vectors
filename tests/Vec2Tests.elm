module Vec2Tests exposing (..)

--import Types exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)
import Types exposing (..)
import Vec2 exposing (Vec2)


vec2 : Test
vec2 =
    describe
        "vec2"
        [ test "behaves like the real constructor" <|
            \_ ->
                Vec2.vec2 True False
                    |> Expect.equal (Vec2 True False)
        ]


map : Test
map =
    des
        << cribe
            "map"
            [ test "maps over all fields" <|
                \_ ->
                    Vec2.map not (Vec2 True False)
                        |> Expect.equal (Vec2 False True)
            ]


pure : Test
pure =
    describe
        "pure"
        [ test "initializes all fields" <|
            \_ ->
                Vec2.pure True
                    |> Expect.equal (Vec2 True True)
        ]


apply : Test
apply =
    describe "apply"
        [ test "applies functions in the context" <|
            \_ ->
                Vec2.apply (Vec2 not not) (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


liftA1 : Test
liftA1 =
    describe "liftA1"
        [ test "lifts and applies unary function" <|
            \_ ->
                Vec2.liftA1 not (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


liftA2 : Test
liftA2 =
    describe "liftA2"
        [ test "lifts and applies binary function" <|
            \_ ->
                Vec2.liftA2 (+) (Vec2 1 2) (Vec2 10 20)
                    |> Expect.equal (Vec2 11 22)
        ]


liftA3 : Test
liftA3 =
    describe "liftA3"
        [ test "lifts and applies ternary function" <|
            \_ ->
                Vec2.liftA3 clamp (Vec2 0 0) (Vec2 100 200) (Vec2 200 300)
                    |> Expect.equal (Vec2 100 200)
        ]



-- bind : Test
-- bind =
--     describe ""
--         [ test "bind" <|
--             \_ ->
--                 Vec2.bind 1
--                     |> Expect.equal ()
--         ]
-- andThen : Test
-- andThen =
--     describe ""
--         [ test "andThen" <|
--             \_ ->
--                 Vec2.andThen 1
--                     |> Expect.equal ()
--         ]
-- return : Test
-- return =
--     describe ""
--         [ test "return" <|
--             \_ ->
--                 Vec2.return 1
--                     |> Expect.equal ()
--         ]
-- foldl : Test
-- foldl =
--     describe ""
--         [ test "foldl" <|
--             \_ ->
--                 Vec2.foldl 1
--                     |> Expect.equal ()
--         ]
-- foldl1 : Test
-- foldl1 =
--     describe ""
--         [ test "foldl1" <|
--             \_ ->
--                 Vec2.foldl1 1
--                     |> Expect.equal ()
--         ]
-- foldr : Test
-- foldr =
--     describe ""
--         [ test "foldr" <|
--             \_ ->
--                 Vec2.foldr 1
--                     |> Expect.equal ()
--         ]
-- foldr1 : Test
-- foldr1 =
--     describe ""
--         [ test "foldr1" <|
--             \_ ->
--                 Vec2.foldr1 1
--                     |> Expect.equal ()
--         ]
-- mapNth0 : Test
-- mapNth0 =
--     describe ""
--         [ test "mapNth0" <|
--             \_ ->
--                 Vec2.mapNth0 1
--                     |> Expect.equal ()
--         ]
-- mapNth1 : Test
-- mapNth1 =
--     describe ""
--         [ test "mapNth1" <|
--             \_ ->
--                 Vec2.mapNth1 1
--                     |> Expect.equal ()
--         ]
-- getNth0 : Test
-- getNth0 =
--     describe ""
--         [ test "getNth0" <|
--             \_ ->
--                 Vec2.getNth0 1
--                     |> Expect.equal ()
--         ]
-- getNth1 : Test
-- getNth1 =
--     describe ""
--         [ test "getNth1" <|
--             \_ ->
--                 Vec2.getNth1 1
--                     |> Expect.equal ()
--         ]
-- toTuple : Test
-- toTuple =
--     describe ""
--         [ test "toTuple" <|
--             \_ ->
--                 Vec2.toTuple 1
--                     |> Expect.equal ()
--         ]
-- fromTuple : Test
-- fromTuple =
--     describe ""
--         [ test "fromTuple" <|
--             \_ ->
--                 Vec2.fromTuple 1
--                     |> Expect.equal ()
--         ]
-- toList : Test
-- toList =
--     describe ""
--         [ test "toList" <|
--             \_ ->
--                 Vec2.toList 1
--                     |> Expect.equal ()
--         ]
-- fromList : Test
-- fromList =
--     describe ""
--         [ test "fromList" <|
--             \_ ->
--                 Vec2.fromList 1
--                     |> Expect.equal ()
--         ]
-- reverse : Test
-- reverse =
--     describe ""
--         [ test "reverse" <|
--             \_ ->
--                 Vec2.reverse 1
--                     |> Expect.equal ()
--         ]
-- asArgs : Test
-- asArgs =
--     describe ""
--         [ test "asArgs" <|
--             \_ ->
--                 Vec2.asArgs 1
--                     |> Expect.equal ()
--         ]
