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



-- apply : Test
-- apply =
--     describe ""
--         [ test "apply" <|
--             \_ ->
--                 Vec2.apply 1
--                     |> Expect.equal ()
--         ]
-- liftaA1 : Test
-- liftaA1 =
--     describe ""
--         [ test "liftaA1" <|
--             \_ ->
--                 Vec2.liftA1 1
--                     |> Expect.equal ()
--         ]
-- liftaA2 : Test
-- liftaA2 =
--     describe ""
--         [ test "liftaA2" <|
--             \_ ->
--                 Vec2.liftA2 1
--                     |> Expect.equal ()
--         ]
-- liftaA3 : Test
-- liftaA3 =
--     describe ""
--         [ test "liftaA3" <|
--             \_ ->
--                 Vec2.liftA3 1
--                     |> Expect.equal ()
--         ]
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
-- combine : Test
-- combine =
--     describe ""
--         [ test "combine" <|
--             \_ ->
--                 Vec2.combine 1
--                     |> Expect.equal ()
--         ]
-- unCombine : Test
-- unCombine =
--     describe ""
--         [ test "unCombine" <|
--             \_ ->
--                 Vec2.unCombine 1
--                     |> Expect.equal ()
--         ]
